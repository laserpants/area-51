{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Pong.TypeChecker 
  where
--  ( runCheck
----  , runCheck2 -- TODO: temp
--  ) where
--
import Control.Arrow ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Newtype.Generics
import Data.Foldable
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Tuple (swap)
import Data.Tuple.Extra (first, firstM, second, secondM)
import Data.Void (Void)
import Debug.Trace
import GHC.Generics
import Pong.Data
import Pong.Lang
import Pong.Util (Fix(..), Name, Map, cata, embed, project, trimLabel, (!?), (<$$>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pong.Util.Env as Env
import qualified Data.Text as Text

newtype Substitution =
  Substitution
    { getSubstitution :: Map Int Type
    }

newtype TypeChecker a =
  TypeChecker
    { getTypeChecker :: ExceptT TypeError (ReaderT (Environment PolyType) (State (Int, Substitution))) a
    }

data TypeError
  = UnificationError
  | NotInScope Name
  | ConstructorNotInScope Name
  | EmptyCaseStatement

runTypeChecker' :: Int -> Environment PolyType -> TypeChecker a -> Either TypeError a
runTypeChecker' n env m = 
  evalState (runReaderT (runExceptT (getTypeChecker m)) env) (n, mempty)

runTypeChecker :: Environment PolyType -> TypeChecker a -> Either TypeError a
runTypeChecker = runTypeChecker' 1

substitute :: Map Int (TypeT t) -> TypeT t -> TypeT t 
substitute sub =
  cata $ \case
    TVar n -> fromMaybe (tVar n) (sub !? n)
    TCon c ts -> tCon c ts 
    TArr t1 t2 -> tArr t1 t2
    TRow row -> tRow (substRow sub row)
    t -> embed t

substRow :: Map Int (TypeT t) -> Row (TypeT t) Int -> Row (TypeT t) Int
substRow sub = 
  cata $ \case
    RNil -> rNil
    RExt name t row -> rExt name (substitute sub t) row
    RVar n -> 
      case project <$> (sub !? n) of
        Just (TRow r) -> r
        _ -> rVar n

class Substitutable a where
  apply :: Substitution -> a -> a

instance Substitutable (Row Type Int) where
  apply = substRow . unpack 

instance Substitutable (Row PolyType Int) where
  apply = substRow . (toPolyType <$>) . unpack 

instance Substitutable Type where
  apply = substitute . unpack

instance Substitutable PolyType where
  apply = substitute . (toPolyType <$>) . unpack

instance (Substitutable t) => Substitutable (Expr t t a1 a2) where
  apply sub =
    cata $ \case
      EVar name -> eVar (subst name)
      ECon con -> eCon (subst con)
      ELet bind expr1 expr2 -> eLet (subst bind) expr1 expr2
      ELam t args expr -> eLam_ t (subst <$> args) expr
      EApp t fun args -> eApp (apply sub t) fun args
      ECase expr cs -> eCase expr (first (fmap subst) <$> cs)
      e -> embed e
    where
      subst = first (apply sub)

instance Substitutable (Map k Type) where
  apply = fmap . apply

instance Substitutable (Environment PolyType) where
  apply = fmap . apply

instance (Substitutable t) => Substitutable (Definition (Label t) (Expr t t a1 a2)) where
  apply sub = \case
    Function args (t, body) ->
      Function (first (apply sub) <$> args) (apply sub t, apply sub body)
    Constant (t, a) ->
      Constant (apply sub t, apply sub a)
    def -> def

compose :: Substitution -> Substitution -> Substitution
compose = over2 Substitution fun where
  fun s1 s2 = apply (Substitution s1) s2 `Map.union` s1

mapsTo :: Int -> Type -> Substitution
mapsTo = pack <$$> Map.singleton

tagLabel :: Name -> TypeChecker (Label Int)
tagLabel name = (,) <$> tag <*> pure name

tagExpr :: Expr t () () Void -> TypeChecker (Expr Int Int () Void)
tagExpr = 
  cata $ \case
    EVar (_, name) -> eVar <$> tagLabel name
    ECon (_, con) -> eCon <$> tagLabel con
    ELit prim -> pure (eLit prim)
    EIf e1 e2 e3 -> eIf <$> e1 <*> e2 <*> e3
    ELet (_, name) e1 e2 -> eLet <$> tagLabel name <*> e1 <*> e2
    EApp _ fun args -> eApp <$> tag <*> fun <*> sequence args
    ELam _ args expr -> eLam <$> traverse (tagLabel . snd) args <*> expr
    EOp2 op e1 e2 -> eOp2 op <$> e1 <*> e2
    ECase e1 cs ->
      eCase <$> e1 <*> traverse (firstM (traverse (tagLabel . snd)) <=< sequence) cs
    ERow row -> eRow <$> tagRow row
  where
    tagRow = 
      cata $ \case
        RNil -> pure rNil
        RVar (_, var) -> rVar <$> tagLabel var
        RExt name expr row -> rExt name <$> tagExpr expr <*> row

tag :: MonadState (Int, a) m => m Int
tag = do
  (s, a) <- get
  put (succ s, a)
  pure s

--typeCheck :: SourceExpr t -> Compiler (Either TypeError TypedExpr)
--typeCheck ast = asks (`runCheck` ast)

unifyAndCombine :: Type -> Type -> Substitution -> TypeChecker Substitution
unifyAndCombine t1 t2 sub1 = do
  sub2 <- unify (apply sub1 t1) (apply sub1 t2)
  pure (sub2 <> sub1)

unifyTypes :: [Type] -> [Type] -> TypeChecker Substitution
unifyTypes ts1 ts2 = foldrM (uncurry unifyAndCombine) mempty (zip ts1 ts2)

unifyRows :: Row Type Int -> Row Type Int -> TypeChecker Substitution
unifyRows r1 r2 =
  case (unwindRow r1, unwindRow r2) of
    ((m1, Fix RNil), (m2, Fix RNil)) 
      | Map.null m1 && Map.null m2 -> pure mempty
    ((m1, Fix (RVar r)), (m2, k)) 
      | Map.null m1 && not (Map.null m2) && k == rVar r -> throwError UnificationError
      | Map.null m1 -> pure (r `mapsTo` tRow r2)
    ((m1, j), (m2, Fix (RVar r))) 
      | Map.null m2 && not (Map.null m1) && j == rVar r -> throwError UnificationError
      | Map.null m2 -> pure (r `mapsTo` tRow r1)

--    ((m1, Fix (RVar r)), (m2, k)) | Map.null m1 -> 
--      if not (Map.null m2) && k == rVar r
--         then throwError UnificationError
--         else pure (r `mapsTo` tRow r2)
--    ((m1, j), (m2, Fix (RVar r))) | Map.null m2 -> 
--      if not (Map.null m1) && j == rVar r
--         then throwError UnificationError
--         else pure (r `mapsTo` tRow r1)

    ((m1, j), (m2, k)) 
      | Map.null m1 -> unifyRows r2 r1
      | otherwise ->
        case Map.lookup a m2 of
          Just (u:us) -> do
            let r1 = foldRow j (updateMap m1 ts)
                r2 = foldRow k (updateMap m2 us)
            unifyTypes [tRow r1, t] [tRow r2, u]
          _ 
            | k == j -> throwError UnificationError
            | otherwise -> do
              p <- rVar <$> tag
              let r1 = foldRow j (updateMap m1 ts)
                  r2 = foldRow p m2
              unifyTypes [tRow r1, tRow k] [tRow r2, tRow (rExt a t p)]
        where
          (a, t:ts) = Map.elemAt 0 m1
          updateMap m = 
            \case
              [] -> Map.delete a m
              ts -> Map.insert a ts m

--  case (project r1, project r2) of
--    (RNil, RNil) -> pure mempty
--    (RVar r, row) -> pure (r `mapsTo` tRow r2) 
--    (row, RVar r) -> pure (r `mapsTo` tRow r1)
--    _ -> foo (unwindRow r1, unwindRow r2)
--  where
--    foo = \case 
--      (q1@(m1, _), q2) | Map.null m1 -> foo (q2, q1)
--      ((m1, j), (m2, k)) -> do
--        case Map.lookup a m2 of
--          Just (u:us) -> do
--            let r1 = foldRow j (updateMap m1 ts)
--                r2 = foldRow k (updateMap m2 us)
--            unifyTypes [tRow r1, t] [tRow r2, u]
--          _ | j == k -> throwError UnificationError
--            | otherwise -> 
--              undefined
--                --unifyTypes [foldRow j (updateMap m1 ts), t] [foldRow k (updateMap m2 us), u]
--        where
--          (a, t:ts) = Map.elemAt 0 m1
--          updateMap m = 
--            \case
--              [] -> Map.delete a m
--              ts -> Map.insert a ts m


--unifyRows r1 r2 =
--  case (unwindRow r1, unwindRow r2) of
--    ((m1, j), (m2, k)) | Map.null m1 && Map.null m2 -> 
--      case (project j, project k) of
--        (RNil, RNil) -> pure mempty
--        (RVar r, row) -> pure (r `mapsTo` tRow r2) 
--        (row, RVar r) -> pure (r `mapsTo` tRow r1)
--    ((m1, _), _) | Map.null m1 -> unifyRows r2 r1
--    ((m1, j), (m2, k)) -> do
--      traceShowM a
--      traceShowM (j, k)
--      case Map.lookup a m2 of
--        Just (u:us) -> unifyTypes [foldRow j (updateMap m1 ts), t] [foldRow k (updateMap m2 us), u]
--        _ | j == k -> throwError UnificationError
--          | otherwise -> 
--            undefined
--              --unifyTypes [foldRow j (updateMap m1 ts), t] [foldRow k (updateMap m2 us), u]
--      where
--        (a, t:ts) = Map.elemAt 0 m1
--        updateMap m = 
--          \case
--            [] -> Map.delete a m
--            ts -> Map.insert a ts m

--                    combinePairs
--                        (foldRow j (updateMap m1 ts), t)
--                        (foldRow k (updateMap m2 us), u)
--
--                    s <- supply
--                    let tv = tVar kRow ("$r" <> showt s)
--                    combinePairs
--                        (foldRow j (updateMap m1 ts), k)
--                        (foldRow tv m2, tRow a t tv)

unify :: Type -> Type -> TypeChecker Substitution
unify t1 t2 =
  case (project t1, project t2) of
    (TVar n, t) -> pure (n `mapsTo` embed t)
    (t, TVar n) -> pure (n `mapsTo` embed t)
    (TCon c1 ts1, TCon c2 ts2) | c1 == c2 -> unifyTypes ts1 ts2
    (TArr t1 t2, TArr u1 u2) -> unifyTypes [t1, t2] [u1, u2]
    (TRow r, TRow s) -> unifyRows r s
    _ 
      | t1 == t2 -> pure mempty
      | otherwise -> throwError UnificationError

applySubstitution :: (Substitutable a) => a -> TypeChecker a
applySubstitution a = gets (apply . snd) <*> pure a

unifyM :: (Substitutable a, Substitutable b, Typed a, Typed b) => a -> b -> TypeChecker ()
unifyM a b = do
  sub <- gets snd
  sub1 <- unify (typeOf (apply sub a)) (typeOf (apply sub b))
  modify (second (sub1 <>))

--runCheck :: TypeEnv -> SourceExpr t -> Either TypeError TypedExpr 
--runCheck symtab ast = do
----  let (Substitution x) = sub in flip mapM_ (Map.toList x) $ \(a, b) ->
----    traceShowM (show a <> " :: " <> show b)
--  apply sub <$> res
--  where
--    (res, (_, sub)) = runMonad (check =<< tagExpr ast)
--    runMonad m =
--      runState (runReaderT (runExceptT (getTypeChecker m)) symtab) (1, mempty)
--
------ TODO: temp
------runCheck2 :: TypeEnv -> SourceExpr t -> Either TypeError TypedExpr 
----runCheck2 symtab ast = res
----  where
----    (res, (_, sub)) = runMonad (tagExpr ast)
----    runMonad m =
----      runState (runReaderT (runExceptT (getTypeChecker m)) symtab) (1, mempty)

--checkName :: (Int, Name) -> (Name -> TypeError) -> TypeChecker (Type, Name)
--checkName (t, var) err = do
--  Env env <- ask
--  case env !? var of
--    Just s -> do
--      t1 <- instantiate s
--      traceShowM t1
----      unifyM (tVar t :: Type) t1
--      pure (t1, var)
--    _ ->
--      throwError (err var)

instantiate :: PolyType -> TypeChecker Type
instantiate p = do
  ts <- traverse (const tag) (Set.toList (bound p))
  pure (fromPolyType (tVar <$> ts) p)
  where
    bound :: PolyType -> Set Int
    bound = cata $ \case
      TGen n -> Set.singleton n
      TCon _ ts -> Set.unions ts
      TArr t1 t2 -> Set.union t1 t2
      TRow r -> (`cata` r) (\case
        RExt _ r a -> Set.union (bound r) a
        _ -> mempty)
      _ -> mempty

generalize :: Type -> TypeChecker PolyType
generalize t = do
  env <- ask
  t1 <- applySubstitution t
  e1 <- applySubstitution env
  let names = filter (`notElem` free e1) (free t1)
      ixs = Map.fromList (zip names [0..])
  pure (substitute (tGen <$> ixs) (toPolyType t1))

checkName :: (Int, Name) -> (Name -> TypeError) -> TypeChecker (Type, Name)
--checkName (t, "{b}") _ = do 
--  r <- tag
--  t0 <- tag
--  t1 <- instantiate (tVar t0 ~> tRow (rVar r) ~> tRow (rExt "b" (tVar t0) (rVar r)))
--  unifyM (tVar t :: Type) t1
--  pure (t1, "{b}")
checkName (t, var) err = do
  Environment env <- ask
  case env !? var of
    Just s -> do
      t1 <- instantiate s
      unifyM (tVar t :: Type) t1
      pure (t1, var)
    _ ->
      throwError (err var)

check :: Expr Int Int () Void -> TypeChecker TypedExpr
check = 
  cata $ \case
    EVar var -> eVar <$> checkName var NotInScope 
    ECon con -> eCon <$> checkName con ConstructorNotInScope 
    ELit prim -> pure (eLit prim)
    EIf expr1 expr2 expr3 -> do
      e1 <- expr1
      unifyM (tBool :: Type) e1 
      e2 <- expr2
      e3 <- expr3
      unifyM e2 e3
      pure (eIf e1 e2 e3)
    ELet (t, var) expr1 expr2 -> do
      e1 <- local (Env.insert var (tGen 0)) expr1 -- TODO: ???
      s <- generalize (typeOf e1)
      e2 <- local (Env.insert var s) expr2
      unifyM (tVar t :: Type) e1
      t0 <- applySubstitution (tVar t)
      pure (eLet (t0, var) e1 e2)
    EApp t fun args -> do
      f <- fun
      as <- sequence args
      t0 <- applySubstitution (typeOf f)
      t1 <- applySubstitution (tVar t)
      unifyM t0 (foldType t1 (typeOf <$> as))
      pure (eApp t1 f as)
    ELam _ args expr -> do
      xs <- traverse (pure . first tVar) args
      e <- local (insertArgs (first (toPolyType . tVar) <$> args)) expr
      pure (eLam xs e)
    EOp2 op expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      let [t1, t2] = argTypes op
      unifyM e1 t1
      unifyM e2 t2
      pure (eOp2 op e1 e2)
    ECase _ [] -> 
      throwError EmptyCaseStatement
    ECase expr clauses -> do
      e <- expr
      case project e of
        ERow row -> do
            c <- checkRowCase row (head clauses)
            pure (eCase e [c])
        _ -> do
            cs <- traverse (secondM applySubstitution <=< checkCase) clauses
            let t:ts = snd <$> cs
            forM_ ts (unifyM t)
            pure (eCase e cs)
    ERow row -> 
      eRow <$> checkRow row

checkRowCase :: Row TypedExpr (Label Type) -> ([Label Int], TypeChecker TypedExpr) -> TypeChecker ([Label Type], TypedExpr)
-- TODO: use case
checkRowCase (Fix RNil) ([], expr) = do
  e <- expr
  pure ([], e)
checkRowCase (Fix (RVar v)) ([(_, _)], expr) = do
  undefined -- TODO
checkRowCase row ([(t1, label), (t2, v), (t3, r)], expr) = do
  let a = trimLabel label
      (e, q) = splitRow a row
      vars =
        [ (typeOf e ~> typeOf q ~> typeOf row, label)
        , (typeOf e, v)
        , (typeOf q, r)
        ]
  xx <- local (Env.inserts (toPolyType <$$> swap <$> vars)) expr
  pure (vars, xx)

checkCase 
  :: ([Label Int], TypeChecker TypedExpr) 
  -> TypeChecker ([Label Type], TypedExpr)
checkCase (con:vs, expr) = do
  (t, _) <- checkName con ConstructorNotInScope 
  let ts = unwindType t
      ps = (snd <$> vs) `zip` (toPolyType <$> ts)
  e <- local (Env.inserts ps) expr
  tvs <-
    forM (zip vs ts) $ \((t, n), t1) -> do
      unifyM (tVar t :: Type) t1
      pure (tVar t, n)
  pure ((t, snd con) : tvs, e)

checkRow 
  :: Row (Expr Int Int () Void) (Label Int) 
  -> TypeChecker (Row TypedExpr (Label Type))
checkRow = cata $ \case
  RNil -> pure rNil 
  RVar var -> rVar <$> checkName var NotInScope
  RExt name expr row -> rExt name <$> check expr <*> row

--checkCase :: ([Label Int], TypeChecker TypedExpr) -> TypeChecker ([Label Type], TypedExpr)
--checkCase ((_, con):vs, expr) = do
--  Env env <- ask
--  ty <- maybe (throwError (NotInScope con)) pure (env !? con)
--  let ts = unwindType ty
--      ps = vs `zip` ts
--  e <- local (Env.inserts (first snd <$> ps)) expr
--  tvs <-
--    forM ps $ \((t, n), t1) -> do
--      unifyM (tVar t) t1
--      pure (tVar t, n)
--  pure ((ty, con) : tvs, e)

--check :: SourceExpr Int -> TypeChecker TypedExpr
--check =
--  cata $ \case
--    -- TODO: generalization?
--    ELet _ (t, name) expr1 expr2 -> do
--      let insertBound e = do
--            ty <- applySubstitution (tVar t)
--            local (Env.insert name ty) e
--      e1 <- insertBound expr1
--      unifyM (tVar t) e1
--      e2 <- insertBound expr2
--      t1 <- applySubstitution (tVar t)
--      pure (let_ (t1, name) e1 e2)
--    EApp _ fun args -> do
--      f <- fun
--      as <- sequence args
--      t1 <- applySubstitution (typeOf f)
--      let ps = zip (unwindType t1) (typeOf <$> as)
--      forM_ ps (uncurry unifyM)
--      pure (app f as)
--    ELam _ args expr -> do
--      e <- local (insertArgs (first tVar <$> args)) expr
--      xs <- traverse (firstM (applySubstitution . tVar)) args
--      pure (lam xs e)
--    EVar (t, name) -> do
--      Env env <- ask
--      case env !? name of
--        Just t1 -> do
--          unifyM (tVar t) t1
--          pure (var (t1, name))
--        _ -> throwError (NotInScope name)
--    ECase _ [] -> throwError EmptyCaseStatement
--    ECase expr clauses -> do
--      e <- expr
--      cs <- traverse (checkCase <=< applySubstitution) clauses
--      let t:ts = snd <$> cs
--      forM_ ts (unifyM t)
--      pure (case_ e cs)
--    ELit prim -> pure (lit prim)
--    EIf expr1 expr2 expr3 -> do
--      e1 <- expr1
--      e2 <- expr2
--      e3 <- expr3
--      unifyM e1 tBool
--      unifyM e2 e3
--      pure (if_ e1 e2 e3)
--    EOp2 op expr1 expr2 -> do
--      e1 <- expr1
--      e2 <- expr2
--      let [t1, t2, _] = unwindType (typeOf op)
--      unifyM e1 t1
--      unifyM e2 t2
--      pure (op2 op e1 e2)
--
--checkCase :: ([Label Int], TypeChecker TypedExpr) -> TypeChecker ([Label Type], TypedExpr)
--checkCase ((_, con):vs, expr) = do
--  Env env <- ask
--  ty <- maybe (throwError (NotInScope con)) pure (env !? con)
--  let ts = unwindType ty
--      ps = vs `zip` ts
--  e <- local (Env.inserts (first snd <$> ps)) expr
--  tvs <-
--    forM ps $ \((t, n), t1) -> do
--      unifyM (tVar t) t1
--      pure (tVar t, n)
--  pure ((ty, con) : tvs, e)

-- Substitution
instance Semigroup Substitution where
  (<>) = compose

deriving instance Monoid Substitution 

deriving instance Show Substitution

deriving instance Eq Substitution

deriving instance Ord Substitution

deriving instance Generic Substitution

instance Newtype Substitution

-- TypeChecker
deriving instance Functor TypeChecker

deriving instance Applicative TypeChecker

deriving instance Monad TypeChecker

deriving instance (MonadState (Int, Substitution)) TypeChecker

deriving instance (MonadReader (Environment PolyType)) TypeChecker

deriving instance (MonadError TypeError) TypeChecker

deriving instance MonadFix TypeChecker

deriving instance Generic (TypeChecker a)

instance Newtype (TypeChecker a)

-- TypeError
deriving instance Show TypeError

deriving instance Eq TypeError
