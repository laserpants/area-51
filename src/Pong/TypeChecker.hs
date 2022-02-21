{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pong.TypeChecker 
  where
--  ( runCheck
----  , runCheck2 -- TODO: temp
--  ) where
--
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (nub)
import Data.Void (Void)
import Data.Foldable
import Data.Set (Set)
import Debug.Trace
--import Debug.Trace
--import Control.Arrow ((>>>))
--import Control.Monad.Except
--import Control.Monad.Reader
--import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (first, firstM, second)
import Pong.Data
import Pong.Lang
import Pong.Util
--import qualified Data.Map.Strict as Map
--import qualified Pong.Util.Env as Env
--
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pong.Util.Env as Env

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

runTypeChecker :: Environment PolyType -> TypeChecker a -> Either TypeError a
runTypeChecker env m = 
  evalState (runReaderT (runExceptT (getTypeChecker m)) env) (1, mempty)

substitute :: Map Int (TypeT t) -> TypeT t -> TypeT t 
substitute sub =
  cata $ \case
    TVar n -> fromMaybe (embed1 TVar n) (sub !? n)
    TCon c ts -> embed2 TCon c ts 
    TArr t1 t2 -> embed2 TArr t1 t2
    TRow r -> undefined -- TODO
    t -> embed t

class Substitutable a where
  apply :: Substitution -> a -> a

instance Substitutable Type where
  apply = substitute . getSubstitution 

instance Substitutable PolyType where
  apply = substitute . (toPolyType <$>) . getSubstitution 

instance (Substitutable t) => Substitutable (Expr t () () a2) where
  apply sub =
    cata $ \case
      EVar name -> eVar (subst name)
      ECon con -> eCon (subst con)
      ELet bind expr1 expr2 -> eLet (subst bind) expr1 expr2
      ELam _ args expr -> eLam (subst <$> args) expr
      EApp _ fun args -> eApp () fun args
      ECase expr cs -> eCase expr (first (fmap subst) <$> cs)
      e -> embed e
    where
      subst = first (apply sub)

instance (Functor f, Substitutable a) => Substitutable (f a) where
  apply = fmap . apply

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 =
  Substitution
    (fmap (apply s1) (getSubstitution s2) `Map.union` getSubstitution s1)

mapsTo :: Int -> Type -> Substitution
mapsTo = Substitution <$$> Map.singleton

--tagLabel :: Name -> TypeChecker (Label Type)
--tagLabel name = do -- (,) <$> tag <*> pure name
--  t <- tag
--  pure (tVar t, name)

--tagExpr = undefined

--tagExpr :: Expr t () Type Void -> TypeChecker (Expr Type () Type Void)
--tagExpr = 
--  cata $ \case
--    EVar (_, name) -> eVar <$> tagLabel name
--    ECon (_, con) -> eCon <$> tagLabel con
--    ELit prim -> pure (eLit prim)
--    EIf e1 e2 e3 -> eIf <$> e1 <*> e2 <*> e3
--    ELet (_, name) e1 e2 -> eLet <$> tagLabel name <*> e1 <*> e2
--    ELam _ args expr -> eLam <$> traverse (tagLabel . snd) args <*> expr
--    EApp _ fun args -> do
--      t0 <- tVar <$> tag
--      t1 <- tVar <$> tag
--      f <- fun
--      as <- sequence args
--      unifyM (t0 ~> t1 :: Type) f
--      pure (eApp f as)
--    EOp2 op e1 e2 -> eOp2 op <$> e1 <*> e2
--    ECase e1 cs ->
--      eCase <$> e1 <*> traverse (firstM (traverse (tagLabel . snd)) <=< sequence) cs
--    ERow row -> eRow <$> tagRow row

--tagRow :: Row (Expr t () Type Void) (Label t) -> TypeChecker (Row (Expr Type () Type Void) (Label Type))
--tagRow = cata $ \case
--  RNil -> pure rNil
--  RVar (_, var) -> rVar <$> tagLabel var
--  RExt name expr row -> rExt name <$> tagExpr expr <*> row

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

--tagExpr :: Expr t () () Void -> TypeChecker (Expr Int () () Void)
--tagExpr =
--  cata $ \case
--    EVar (_, name) -> eVar <$> tagLabel name
--    ECon (_, con) -> eCon <$> tagLabel con
--    ELit prim -> pure (eLit prim)
--    EIf e1 e2 e3 -> eIf <$> e1 <*> e2 <*> e3
--    ELet (_, name) e1 e2 -> eLet <$> tagLabel name <*> e1 <*> e2
--    ELam _ args expr -> eLam <$> traverse (tagLabel . snd) args <*> expr
--    EApp _ fun args -> eApp <$> fun <*> sequence args
--    EOp2 op e1 e2 -> eOp2 op <$> e1 <*> e2
--    ECase e1 cs ->
--      eCase <$> e1 <*> traverse (firstM (traverse (tagLabel . snd)) <=< sequence) cs
--    ERow row -> eRow <$> tagRow row

tagRow :: Row (Expr t () () Void) (Label t) -> TypeChecker (Row (Expr Int Int () Void) (Label Int))
tagRow = cata $ \case
  RNil -> pure rNil
  RVar (_, var) -> rVar <$> tagLabel var
  RExt name expr row -> rExt name <$> tagExpr expr <*> row

tag :: MonadState (Int, a) m => m Int
tag = do
  (s, a) <- get
  put (succ s, a)
  pure s

--xx1 :: Expr Int () () Void -> TypeChecker (Expr Type () () Void)
--xx1 = 
--  cata $ \case
--    EVar (t, var) -> pure (eVar (tVar t, var))
--    EApp _ fun args -> do
--      f <- fun
--      as <- sequence args
--      t0 <- tVar <$> tag
--      t1 <- tVar <$> tag
--      unifyM (t0 ~> t1) f
--      pure (eApp f as)
--    e -> undefined

--typeCheck :: SourceExpr t -> Compiler (Either TypeError TypedExpr)
--typeCheck ast = asks (`runCheck` ast)

unifyAndCombine :: (MonadError TypeError m) => Type -> Type -> Substitution -> m Substitution
unifyAndCombine t1 t2 sub1 = do
  sub2 <- unify (apply sub1 t1) (apply sub1 t2)
  pure (sub2 <> sub1)

unifyTypes :: (MonadError TypeError m) => [Type] -> [Type] -> m Substitution
unifyTypes ts1 ts2 = foldrM (uncurry unifyAndCombine) mempty (zip ts1 ts2)

unify :: (MonadError TypeError m) => Type -> Type -> m Substitution
unify t1 t2 =
  case (project t1, project t2) of
    (TVar n, t) -> pure (n `mapsTo` embed t)
    (t, TVar n) -> pure (n `mapsTo` embed t)
    (TCon c1 ts1, TCon c2 ts2) | c1 == c2 -> unifyTypes ts1 ts2
    (TArr t1 t2, TArr u1 u2) -> unifyTypes [t1, t2] [u1, u2]
--      sub1 <- unify t1 u1
--      sub2 <- unify (apply sub1 t2) (apply sub1 u2)
--      pure (sub2 <> sub1)
    _
      | t1 == t2 -> pure mempty
      | otherwise -> throwError UnificationError

applySubstitution ::
     (MonadState (Int, Substitution) m, Substitutable a) => a -> m a
applySubstitution a = gets (apply . snd) <*> pure a

unifyM ::
     ( MonadError TypeError m
     , MonadState (Int, Substitution) m
     , Substitutable a
     , Substitutable b
     , Typed a
     , Typed b
     )
  => a
  -> b
  -> m ()
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

checkName :: (Type, Name) -> (Name -> TypeError) -> TypeChecker (Type, Name)
checkName (t, var) err = do
  Env env <- ask
  case env !? var of
    Just s -> do
      t1 <- instantiate s
      unifyM t t1
      pure (t1, var)
    _ ->
      throwError (err var)

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

check :: Expr Type () Type Void -> TypeChecker (Expr Type () Type Void)
check = 
  cata $ \case
    EVar var -> eVar <$> checkName var NotInScope 
--    ECon con -> eCon <$> checkName con ConstructorNotInScope 
--    ELit prim -> pure (eLit prim)
--    EIf expr1 expr2 expr3 -> do
--      e1 <- expr1
--      e2 <- expr2
--      e3 <- expr3
--      unifyM e1 (tBool :: Type)
--      unifyM e2 e3
--      pure (eIf e1 e2 e3)
--    ELet (t, var) expr1 expr2 -> do
--      let insertBound e = do
--            ty <- applySubstitution t
--            local (Env.insert var (toPolyType ty)) e
--      e1 <- insertBound expr1
--      unifyM t e1
--      e2 <- insertBound expr2
--      t1 <- applySubstitution t
--      pure (eLet (t1, var) e1 e2)
--    ELam _ args expr -> do
--      e <- local (insertArgs (first toPolyType <$> args)) expr
--      xs <- traverse (firstM applySubstitution) args
--      pure (eLam xs e)
--    EApp _ fun args -> do
--      f <- fun
--      as <- sequence args
--      --unifyM (tx ~> ty) f
--      t0 <- applySubstitution (typeOf f)
--      --traceShowM "////"
--      --traceShowM t0
--      --traceShowM (argTypes t0)
--      --unifyM tx (typeOf <$> as)
--      t1 <- applySubstitution t0
--      let ps = zip (argTypes t1) (typeOf <$> as)
--      --let ps = zip (argTypes t0) (typeOf <$> as)
--      --traceShowM ps
--      forM_ ps (uncurry unifyM)
--      pure (eApp f as)
--    EOp2 op expr1 expr2 -> do
--      e1 <- expr1
--      e2 <- expr2
--      let [t1, t2] = argTypes op
--      unifyM e1 t1
--      unifyM e2 t2
--      pure (eOp2 op e1 e2)
--    ECase _ [] -> throwError EmptyCaseStatement
--    ECase expr clauses ->
--      undefined
--      -- TODO
--    ERow row ->
--      undefined
--      -- TODO

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
--      cs <- traverse (checkClause <=< applySubstitution) clauses
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
--checkClause :: ([Label Int], TypeChecker TypedExpr) -> TypeChecker ([Label Type], TypedExpr)
--checkClause ((_, con):vs, expr) = do
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

instance Monoid Substitution where
  mempty = Substitution mempty

deriving instance Show Substitution

deriving instance Eq Substitution

deriving instance Ord Substitution

-- TypeChecker
deriving instance Functor TypeChecker

deriving instance Applicative TypeChecker

deriving instance Monad TypeChecker

deriving instance (MonadState (Int, Substitution)) TypeChecker

deriving instance (MonadReader (Environment PolyType)) TypeChecker

deriving instance (MonadError TypeError) TypeChecker

-- TypeError
deriving instance Show TypeError

deriving instance Eq TypeError
