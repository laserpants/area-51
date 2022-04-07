{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Pong.TypeChecker where

import Control.Arrow ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Newtype.Generics
import Data.Foldable
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import TextShow (showt)
import qualified Data.Text as Text
import Data.Tuple.Extra (first, firstM, second, secondM)
import Data.Void (Void)
import Debug.Trace
import GHC.Generics (Generic)
import Pong.Data
import Pong.Lang
import Pong.Util
  ( Fix(..)
  , Map
  , Name
  , (!?)
  , (<$$>)
  , cata
  , embed
  , project
  , trimLabel
  )
import qualified Pong.Util.Env as Env

newtype Substitution =
  Substitution (Map Int Type)

newtype TypeChecker a =
  TypeChecker
    (ExceptT TypeError (ReaderT (Environment Type) (State ( Int
                                                              , Substitution))) a)

data TypeError
  = UnificationError
  | NotInScope Name
  | ConstructorNotInScope Name
  | IllFormedExpression

runTypeChecker' ::
     Int -> Environment Type -> TypeChecker a -> Either TypeError a
runTypeChecker' n env m =
  evalState (runReaderT (runExceptT (unpack m)) env) (n, mempty)

runTypeChecker :: Environment Type -> TypeChecker a -> Either TypeError a
runTypeChecker = runTypeChecker' 1

substitute :: Map Int Type -> Type -> Type
substitute sub =
  cata $ \case
    TVar n -> fromMaybe (tVar n) (sub !? n)
    TCon c ts -> tCon c ts
    TArr t1 t2 -> tArr t1 t2
    TRow row -> tRow (rowSubstitute sub row)
    t -> embed t

rowSubstitute :: Map Int Type -> Row Type Int -> Row Type Int
rowSubstitute sub =
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
  apply = rowSubstitute . unpack

--instance Substitutable (Row PolyType Int) where
--  apply = rowSubstitute . (toPolyType <$>) . unpack

instance Substitutable Type where
  apply = substitute . unpack

--instance Substitutable PolyType where
--  apply = substitute . (toPolyType <$>) . unpack

instance Substitutable Void where
  apply _ void = void

instance (Substitutable t, Substitutable a0) => Substitutable (Expr t a0 a1 a2) where
  apply sub =
    cata $ \case
      EVar name -> eVar (subst name)
      ECon con -> eCon (first (apply sub) con)
      ELet bind expr1 expr2 -> eLet (subst bind) expr1 expr2
      ELam t args expr -> eLam t (subst <$> args) expr
      EApp t fun args -> eApp (apply sub t) fun args
      ECase expr cs -> eCase expr (first (fmap subst) <$> cs)
      EOp2 (Op2 op t) expr1 expr2 -> eOp2 (Op2 op (apply sub t)) expr1 expr2
      EField field expr1 expr2 -> eField (subst <$> field) expr1 expr2
      ERow row -> eRow (mapRow (apply sub) row)
      e -> embed e
    where
      subst = first (apply sub)

instance (Substitutable t) =>
         Substitutable (Row (Expr t t a1 a2) (Label t)) where
  apply = mapRow . apply

instance Substitutable (Map k Type) where
  apply = fmap . apply

instance Substitutable (Environment Type) where
  apply = fmap . apply

instance Substitutable Constructor where
  apply sub (Constructor name fields) = Constructor name (apply sub <$> fields)

instance (Substitutable t, Substitutable a0) =>
         Substitutable (Definition (Label t) (Expr t a0 a1 a2)) where
  apply sub =
    \case
      Function args (t, body) -> Function (first (apply sub) <$> args) (apply sub t, apply sub body)
      Constant (t, expr) -> Constant (apply sub t, apply sub expr)
      Data name ctors -> Data name (apply sub <$> ctors)
      External args (t, name) -> External (apply sub <$> args) (apply sub t, name)

compose :: Substitution -> Substitution -> Substitution
compose = over2 Substitution fun
  where
    fun s1 s2 = apply (Substitution s1) s2 `Map.union` s1

mapsTo :: Int -> Type -> Substitution
mapsTo = pack <$$> Map.singleton

tagLabel :: Name -> TypeChecker (Label Int)
tagLabel name = (,) <$> tag <*> pure name

tagExpr :: Expr t () () Void -> TypeChecker TaggedExpr
tagExpr =
  cata $ \case
    EVar (_, name) -> eVar <$> tagLabel name
    ECon (_, con) -> eCon <$> tagLabel con
    ELit prim -> pure (eLit prim)
    EIf e1 e2 e3 -> eIf <$> e1 <*> e2 <*> e3
    ELet (_, name) e1 e2 -> eLet <$> tagLabel name <*> e1 <*> e2
    EApp _ fun args -> eApp <$> tag <*> fun <*> sequence args
    ELam _ args expr -> eLam () <$> traverse (tagLabel . snd) args <*> expr
    EOp2 op e1 e2 -> eOp2 <$> tagOp op <*> e1 <*> e2
    ECase e1 cs ->
      eCase <$> e1 <*>
      traverse (firstM (traverse (tagLabel . snd)) <=< sequence) cs
    ERow row -> eRow <$> tagRow row
    EField f e1 e2 -> eField <$> traverse (tagLabel . snd) f <*> e1 <*> e2

tagOp :: Op2 t -> TypeChecker (Op2 Int)
tagOp (Op2 op _) = Op2 op <$> tag

tagRow ::
     Row (Expr t () () Void) (Label t)
  -> TypeChecker (Row TaggedExpr (Label Int))
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

unifyAndCombine :: Type -> Type -> Substitution -> TypeChecker Substitution
unifyAndCombine t1 t2 sub1 = do
  sub2 <- unify (apply sub1 t1) (apply sub1 t2)
  pure (sub2 <> sub1)

unifyTypes :: [Type] -> [Type] -> TypeChecker Substitution
unifyTypes ts us = foldrM (uncurry unifyAndCombine) mempty (zip ts us)

unifyRows :: Row Type Int -> Row Type Int -> TypeChecker Substitution
unifyRows r1 r2 =
  case (unwindRow r1, unwindRow r2) of
    ((m1, Fix RNil), (m2, Fix RNil))
      | Map.null m1 && Map.null m2 -> pure mempty
    ((m1, Fix (RVar r)), (m2, k))
      | Map.null m1 && not (Map.null m2) && k == rVar r ->
        throwError UnificationError
      | Map.null m1 -> pure (r `mapsTo` tRow r2)
    ((m1, j), (m2, Fix (RVar r)))
      | Map.null m2 && not (Map.null m1) && j == rVar r ->
        throwError UnificationError
      | Map.null m2 -> pure (r `mapsTo` tRow r1)
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
      where (a, t:ts) = Map.elemAt 0 m1
            updateMap m =
              \case
                [] -> Map.delete a m
                ts -> Map.insert a ts m

unify :: Type -> Type -> TypeChecker Substitution
unify t1 t2 =
  case (project t1, project t2) of
    (TVar n, t) -> pure (n `mapsTo` embed t)
    (t, TVar n) -> pure (n `mapsTo` embed t)
    (TCon c1 ts1, TCon c2 ts2)
      | c1 == c2 -> unifyTypes ts1 ts2
    (TArr t1 t2, TArr u1 u2) -> unifyTypes [t1, t2] [u1, u2]
    (TRow r, TRow s) -> unifyRows r s
    _
      | t1 == t2 -> pure mempty
      | otherwise -> throwError UnificationError

applySubstitution :: (Substitutable a) => a -> TypeChecker a
applySubstitution a = gets (apply . snd) <*> pure a

unifyM ::
     (Substitutable a, Substitutable b, Typed a, Typed b)
  => a
  -> b
  -> TypeChecker ()
unifyM a b = do
  sub <- gets snd
  sub1 <- unify (typeOf (apply sub a)) (typeOf (apply sub b))
  modify (second (sub1 <>))

instantiate :: Type -> TypeChecker Type
instantiate p = do
  ts <- traverse (\n -> tag >>= \t -> pure (n, tVar t)) (Set.toList (bound p))
  pure (fromPolyType (Map.fromList ts) p)
  where
    bound :: Type -> Set Name
    bound =
      cata $ \case
        TGen n -> Set.singleton n
        TCon _ ts -> Set.unions ts
        TArr t1 t2 -> Set.union t1 t2
        TRow r ->
          (`cata` r)
            (\case
               RExt _ r a -> Set.union (bound r) a
               _ -> mempty)
        _ -> mempty

generalize :: Type -> TypeChecker Type
generalize t = do
  env <- ask
  t1 <- applySubstitution t
  e1 <- applySubstitution env
  let names = filter (`notElem` free e1) (free t1)
      ixs = Map.fromList (zip names ffoo123)
  pure (substitute (tGen <$> ixs) t1)
  where
    ffoo123 :: [Name]
    ffoo123 = ["a" <> showt i | i <- [0 :: Int ..]]

checkName :: (Int, Name) -> (Name -> TypeError) -> TypeChecker (Type, Name)
checkName (t, var) toErr = 
  asks (Env.lookup var) >>= 
    \case
      Just s -> do
        t1 <- instantiate s
        unifyM (tVar t :: Type) t1
        pure (t1, var)
      _ -> throwError (toErr var)

check :: TaggedExpr -> TypeChecker TypedExpr
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
      e1 <- local (Env.insert var (tGen "a0")) expr1 -- TODO: ???
      s <- generalize (typeOf e1)
      e2 <- local (Env.insert var s) expr2
      unifyM (tVar t :: Type) e1
      t0 <- applySubstitution (tVar t)
      pure (eLet (t0, var) e1 e2)
    EApp t fun args -> do
      f <- fun
      as <- sequence args
      t0 <- applySubstitution (typeOf f)  -- ????
--      let t0 = (typeOf f)  -- ????
      t1 <- applySubstitution (tVar t)
      unifyM t0 (foldType t1 (typeOf <$> as))
      pure (eApp t1 f as)
    ELam _ args expr -> do
      as <- traverse (pure . first tVar) args
      e <- local (insertArgs (first tVar <$> args)) expr
      pure (eLam () as e)
    EOp2 (Op2 op t) expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      t0 <- instantiate (binopType op)
      let [t1, t2] = argTypes t0
      unifyM e1 t1
      unifyM e2 t2
      t1 <- applySubstitution t0
      pure (eOp2 (Op2 op t1) e1 e2)
    ECase _ [] -> throwError IllFormedExpression
    ECase expr clauses -> do
      e <- expr
      eCase e <$> checkCases e clauses
    ERow row -> eRow <$> checkRow row
    EField field expr1 expr2 -> do
      e1 <- expr1
      let t = typeOf e1
      (f, e2) <- checkRowCase t field expr2
      pure (eField f e1 e2)

--checkCases expr [clause] | isTCon RowT t = do
--  let TRow row = project t
--  c <- checkRowCase row clause
--  pure [c]
--  where t = typeOf expr

checkRowCase ::
     Type
  -> [Label Int] 
  -> TypeChecker TypedExpr
  -> TypeChecker ([Label Type], TypedExpr)
checkRowCase (Fix (TRow row)) args expr = do
  case args of
    [(u0, label), (u1, v1), (u2, v2)] -> do
      let (r1, q) = splitRow (trimLabel label) row
      let [t0, t1, t2] = tVar <$> [u0, u1, u2] :: [Type]
      unifyM t1 r1
      unifyM t2 (tRow q)
      tx0 <- applySubstitution t0
      tx1 <- applySubstitution t1
      tx2 <- applySubstitution t2
      applySubstitution (t1 ~> t2 ~> tRow row) >>= unifyM tx0 
      e <-
        local
          (Env.inserts [(label, tx0), (v1, tx1), (v2, tx2)])
          expr
      pure ([(t0, label), (t1, v1), (t2, v2)], e)

binopType :: Binop -> Type
binopType = \case
  OEq        -> tGen "a0" ~> tGen "a0" ~> tBool
  OAdd       -> tGen "a0" ~> tGen "a0" ~> tGen "a0"
  OSub       -> tGen "a0" ~> tGen "a0" ~> tGen "a0"
  OMul       -> tGen "a0" ~> tGen "a0" ~> tGen "a0"
  ODiv       -> tGen "a0" ~> tGen "a0" ~> tGen "a0"
  OLogicOr   -> tBool ~> tBool ~> tBool
  OLogicAnd  -> tBool ~> tBool ~> tBool

checkCases ::
     TypedExpr
  -> [([Label Int], TypeChecker TypedExpr)]
  -> TypeChecker [([Label Type], TypedExpr)]
checkCases expr clauses = do
  cs <- traverse (secondM applySubstitution <=< uncurry (checkCase (typeOf expr))) clauses
  let t:ts = snd <$> cs
  forM_ ts (unifyM t)
  pure cs

checkCase ::
  Type
  -> [(Int, Name)]
  -> TypeChecker TypedExpr
  -> TypeChecker ([Label Type], TypedExpr)
checkCase tt (con:vs) expr = do
  (t, _) <- checkName con ConstructorNotInScope
  let ts = unwindType t
      ps = (snd <$> vs) `zip` ts
  unifyM tt (last ts)
  e <- local (Env.inserts ps) expr
  tvs <-
    forM (zip vs ts) $ \((t0, n), t1) -> do
      unifyM (tVar t0 :: Type) t1
      pure (tVar t0, n)
  pure ((t, snd con) : tvs, e)

checkRow ::
     Row (Expr Int Int () Void) (Label Int)
  -> TypeChecker (Row TypedExpr (Label Type))
checkRow =
  cata $ \case
    RNil -> pure rNil
    RVar var -> rVar <$> checkName var NotInScope
    RExt name expr row -> rExt name <$> check expr <*> row

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

deriving instance (MonadReader (Environment Type)) TypeChecker

deriving instance (MonadError TypeError) TypeChecker

deriving instance MonadFix TypeChecker

deriving instance Generic (TypeChecker a)

instance Newtype (TypeChecker a)

-- TypeError
deriving instance Show TypeError

deriving instance Eq TypeError
