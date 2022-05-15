{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Pong.TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Newtype.Generics
import Data.Foldable (foldrM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (first, firstM, second, secondM)
import Debug.Trace
import GHC.Generics (Generic)
import Pong.Data
import Pong.Lang
import Pong.Util (Fix (..), Name, Void, cata, embed, project, (!?), (<$$>), (<<<), (>>>))
import Pong.Util.Env
import qualified Pong.Util.Env as Env
import TextShow (showt)

newtype Substitution
  = Substitution (Map Int PolyType)

type TypeEnv = Environment PolyType

newtype TypeChecker a
  = TypeChecker
      (ExceptT TypeError (ReaderT TypeEnv (State (Int, Substitution))) a)

data TypeError
  = UnificationError
  | NotInScope Name
  | ConstructorNotInScope Name
  | IllFormedExpression

-- -- -- runTypeChecker' :: Int -> Environment (Type Int Name) -> TypeChecker a -> Either TypeError a
-- -- -- runTypeChecker' n env m = evalState (runReaderT (runExceptT (unpack m)) env) (n, mempty)
-- -- --
-- -- -- runTypeChecker :: Environment (Type Int Name) -> TypeChecker a -> Either TypeError a
-- -- -- runTypeChecker = runTypeChecker' (1 :: Int)

-- Substitution

substitute :: Map Int (Type Int s) -> Type Int t -> Type Int s
substitute sub =
  cata $
    \case
      TVar n -> fromMaybe (tVar n) (sub !? n)
      TCon c ts -> tCon c ts
      TArr t1 t2 -> tArr t1 t2
      TRow row -> tRow (rowSubstitute sub row)
      TUnit -> tUnit
      TBool -> tBool
      TInt -> tInt
      TFloat -> tFloat
      TDouble -> tDouble
      TChar -> tChar
      TString -> tString
      TGen{} -> error "Implementation error"

rowSubstitute ::
  Map Int (Type Int s) -> Row (Type Int t) Int -> Row (Type Int s) Int
rowSubstitute sub =
  cata $
    \case
      RNil -> rNil
      RExt name t row -> rExt name (substitute sub t) row
      RVar n ->
        case project <$> (sub !? n) of
          Just (TRow r) -> r
          _ -> rVar n

class AnyType t where
  promote :: t -> PolyType

instance AnyType MonoType where
  promote = toPolyType

instance AnyType PolyType where
  promote = id

class Substitutable a where
  apply :: Substitution -> a -> a

instance Substitutable PolyType where
  apply = substitute . unpack

instance Substitutable MonoType where
  apply sub = toMonoType mempty . apply sub . promote

instance (Functor f, Substitutable a) => Substitutable (f a) where
  apply = fmap . apply

instance (Substitutable a0, Substitutable a2, Substitutable t) => Substitutable (Expr t a0 a1 a2) where
  apply sub =
    cata $
      \case
        EVar name -> eVar (subst name)
        ECon con -> eCon (first (apply sub) con)
        ELet bind expr1 expr2 -> eLet (subst bind) expr1 expr2
        ELam t args expr -> eLam t (subst <$> args) expr
        EApp t fun args -> eApp (apply sub t) fun args
        ECase expr cs -> eCase expr (first (fmap subst) <$> cs)
        EOp1 (t, op) expr1 -> eOp1 (apply sub t, op) expr1
        EOp2 (t, op) expr1 expr2 -> eOp2 (apply sub t, op) expr1 expr2
        EField field expr1 expr2 -> eField (subst <$> field) expr1 expr2
        ERow row -> eRow (mapRow (apply sub) row)
        ECall t fun args -> eCall_ (apply sub t) (subst fun) args
        e -> embed e
    where
      subst = first (apply sub)

instance Substitutable (Row PolyType Int) where
  apply = rowSubstitute . unpack

instance Substitutable (Row MonoType Int) where
  apply sub = mapRow (toMonoType mempty) . rowSubstitute (unpack sub)

--  apply sub = toMonoType mempty . apply sub . promote

-- -- -- ----Instance Substitutable (Row MonoType Int) where
-- -- -- ----  apply = rowSubstitute . unpack
-- -- -- ----
-- -- -- ------ --instance Substitutable (Row PolyType Int) where
-- -- -- ------ --  apply = rowSubstitute . (toPolyType <$>) . unpack
-- -- -- --
-- -- -- --instance Substitutable (Type s) where
-- -- -- --  apply = subs . unpack
-- -- -- --
-- -- -- --subs :: Map Int (Type s) -> Type u -> Type s
-- -- -- --subs = undefined
-- -- --
-- -- -- ----instance Substitutable (Type Name) where
-- -- -- ----  apply = substitute2 . unpack
-- -- -- --
-- -- -- ----instance Substitutable (Type Name) where
-- -- -- ----  apply = substitute2 . unpack
-- -- -- --
-- -- -- ----instance Substitutable MonoType where
-- -- -- ----  apply = substitute . unpack
-- -- -- --
-- -- -- ---- --instance Substitutable PolyType where
-- -- -- ---- --  apply = substitute . (toPolyType <$>) . unpack

instance Substitutable Void where
  apply = const id

-- -- -- instance Substitutable () where
-- -- --   apply _ = const ()
-- -- --
-- -- -- --Instance (Substitutable t, Substitutable a0) => Substitutable (Expr t a0 a1 a2) where
-- -- -- --  apply sub =
-- -- -- --    cata $ \case
-- -- -- --      EVar name -> eVar (subst name)
-- -- -- --      ECon con -> eCon (first (apply sub) con)
-- -- -- --      ELet bind expr1 expr2 -> eLet (subst bind) expr1 expr2
-- -- -- --      ELam t args expr -> eLam t (subst <$> args) expr
-- -- -- --      EApp t fun args -> eApp (apply sub t) fun args
-- -- -- --      ECase expr cs -> eCase expr (first (fmap subst) <$> cs)
-- -- -- --      EOp1 (t, op) expr1 -> eOp1 (apply sub t, op) expr1
-- -- -- --      EOp2 (t, op) expr1 expr2 -> eOp2 (apply sub t, op) expr1 expr2
-- -- -- --      EField field expr1 expr2 -> eField (subst <$> field) expr1 expr2
-- -- -- --      ERow row -> eRow (mapRow (apply sub) row)
-- -- -- --      e -> embed e
-- -- -- --    where
-- -- -- --      subst = first (apply sub)
-- -- -- --
-- -- -- ---- instance (Substitutable t) =>
-- -- -- ----          Substitutable (Row (Expr t t a1 a2) (Label t)) where
-- -- -- ----   apply = mapRow . apply
-- -- --
-- -- -- --instance Substitutable (Map k (Type u)) where
-- -- -- --  apply = fmap . apply
-- -- --
-- -- -- --instance Substitutable (Map k s) where
-- -- -- --  apply = fmap . apply
-- -- --
-- -- -- --Instance (Substitutable e) => Substitutable (Environment e) where
-- -- -- --  apply = fmap . apply
-- -- -- --
-- -- -- ---- instance Substitutable Constructor where
-- -- -- ----   apply sub (Constructor name fields) = Constructor name (apply sub <$> fields)
-- -- -- ----
-- -- -- ---- instance (Substitutable t, Substitutable a0) =>
-- -- -- ----          Substitutable (Definition t (Expr t a0 a1 a2)) where
-- -- -- ----   apply sub =
-- -- -- ----     \case
-- -- -- ----       Function args (t, body) -> Function (first (apply sub) <$> args) (apply sub t, apply sub body)
-- -- -- ----       Constant (t, expr) -> Constant (apply sub t, apply sub expr)
-- -- -- ----       Data name ctors -> Data name (apply sub <$> ctors)
-- -- -- ----       External args (t, name) -> External (apply sub <$> args) (apply sub t, name)
-- -- --
-- -- -- instance (Substitutable t, Substitutable (Expr t a0 a1 a2)) =>
-- -- --   Substitutable (Definition t (Expr t a0 a1 a2))
-- -- --     where
-- -- --       apply sub =
-- -- --         \case
-- -- --           Function args (t, body) -> Function (first (apply sub) <$> args) (apply sub t, apply sub body)

compose :: Substitution -> Substitution -> Substitution
compose = over2 Substitution fun
  where
    fun s1 s2 = apply (Substitution s1) s2 `Map.union` s1

mapsTo :: (AnyType t) => Int -> t -> Substitution
mapsTo n = pack <<< Map.singleton n <<< promote

-- Tagging

tagFst :: a -> TypeChecker (Int, a)
tagFst a = do
  t <- tag
  pure (t, a)

tagExpr :: SourceExpr -> TypeChecker TaggedExpr
tagExpr =
  cata $
    \case
      EVar (_, name) -> eVar <$> tagFst name
      ECon (_, con) -> eCon <$> tagFst con
      ELit prim -> pure (eLit prim)
      EIf e1 e2 e3 -> eIf <$> e1 <*> e2 <*> e3
      ELet (_, name) e1 e2 -> eLet <$> tagFst name <*> e1 <*> e2
      EApp _ fun args -> eApp <$> tag <*> fun <*> sequence args
      ELam _ args expr -> eLam () <$> traverse (tagFst . snd) args <*> expr
      EOp1 (_, op) e1 -> eOp1 <$> tagFst op <*> e1
      EOp2 (_, op) e1 e2 -> eOp2 <$> tagFst op <*> e1 <*> e2
      ECase e1 cs ->
        eCase <$> e1
          <*> traverse (firstM (traverse (tagFst . snd)) <=< sequence) cs
      ERow row -> eRow <$> tagRow row
      EField f e1 e2 -> eField <$> traverse (tagFst . snd) f <*> e1 <*> e2

tagRow :: Row SourceExpr (Label t) -> TypeChecker (Row TaggedExpr (Label Int))
tagRow =
  cata $
    \case
      RNil -> pure rNil
      RVar (_, var) -> rVar <$> tagFst var
      RExt name expr row -> rExt name <$> tagExpr expr <*> row

tag :: MonadState (Int, a) m => m Int
tag = do
  (s, a) <- get
  put (succ s, a)
  pure s

-- Unification

unifyAndCombine ::
  (Substitutable (Type Int s), AnyType (Type Int s), Eq s) =>
  Type Int s ->
  Type Int s ->
  Substitution ->
  TypeChecker Substitution
unifyAndCombine t1 t2 sub1 = do
  sub2 <- unify (apply sub1 t1) (apply sub1 t2)
  pure (sub2 <> sub1)

unifyTypes ::
  (Substitutable (Type Int s), AnyType (Type Int s), Eq s) =>
  [Type Int s] ->
  [Type Int s] ->
  TypeChecker Substitution
unifyTypes ts us = foldrM (uncurry unifyAndCombine) mempty (ts `zip` us)

unifyRows ::
  (Substitutable (Type Int s), AnyType (Type Int s), Eq s) =>
  Row (Type Int s) Int ->
  Row (Type Int s) Int ->
  TypeChecker Substitution
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
          Just (u : us) -> do
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
        (a, t : ts) = Map.elemAt 0 m1
        updateMap m =
          \case
            [] -> Map.delete a m
            ts -> Map.insert a ts m

unify ::
  (Substitutable (Type Int s), AnyType (Type Int s), Eq s) =>
  Type Int s ->
  Type Int s ->
  TypeChecker Substitution
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

applySubstitution :: (Substitutable s) => s -> TypeChecker s
applySubstitution s = gets (apply . snd) <*> pure s

unifyM ::
  (Substitutable (Type Int s), AnyType (Type Int s), Eq s) =>
  Type Int s ->
  Type Int s ->
  TypeChecker ()
unifyM t1 t2 = do
  sub <- gets snd
  sub1 <- unify (apply sub t1) (apply sub t2)
  modify (second (sub1 <>))

instantiate :: PolyType -> TypeChecker MonoType
instantiate ty = do
  ts <- traverse (\n -> tag >>= \v -> pure (n, tVar v)) (Set.toList (boundVars ty))
  pure (toMonoType (Map.fromList ts) ty)

generalize :: MonoType -> TypeChecker PolyType
generalize t = do
  env <- ask
  t1 <- applySubstitution t
  let vars = filter (`notElem` free env) (free t1)
      ixs = Map.fromList (vars `zip` ["a" <> showt i | i <- [0 :: Int ..]])
  pure (substitute (tGen <$> ixs) t1)

-- Inference

lookupName :: Label Int -> (Name -> TypeError) -> TypeChecker (MonoType, Name)
lookupName (t, var) toErr =
  asks (Env.lookup var)
    >>= \case
      Just pt -> do
        mt <- instantiate pt
        unifyM (tVar t) mt
        pure (mt, var)
      _ ->
        throwError (toErr var)

inferExpr :: TaggedExpr -> TypeChecker TypedExpr
inferExpr =
  cata $ \case
    EVar var -> eVar <$> lookupName var NotInScope
    ECon con -> eCon <$> lookupName con ConstructorNotInScope
    ELit prim -> pure (eLit prim)
    EIf expr1 expr2 expr3 -> do
      e1 <- expr1
      e2 <- expr2
      e3 <- expr3
      unifyM (typeOf e1) tBool
      unifyM (typeOf e2) (typeOf e3)
      pure (eIf e1 e2 e3)
    ELet (t, var) expr1 expr2 -> do
      fv <- tVar <$> tag
      e1 <- local (Env.insert var fv) expr1
      s <- generalize (typeOf e1)
      e2 <- local (Env.insert var s) expr2
      unifyM (tVar t) (typeOf e1)
      t0 <- applySubstitution (tVar t)
      pure (eLet (t0, var) e1 e2)
    EApp t fun args -> do
      f <- fun
      as <- sequence args
      t1 <- applySubstitution (tVar t)
      unifyM (typeOf f) (foldType t1 (typeOf <$> as))
      pure (eApp t1 f as)
    ELam _ args expr -> do
      as <- traverse (pure . first tVar) args
      e <- local (insertArgs (first tVar <$> args)) expr
      pure (eLam () as e)
    EOp1 (t, op) expr1 -> do
      e1 <- expr1
      t0 <- instantiate (unopType op)
      let [t1] = argTypes t0
      unifyM t1 (typeOf e1)
      ty <- applySubstitution t0
      pure (eOp1 (ty, op) e1)
    EOp2 (t, op) expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      t0 <- instantiate (binopType op)
      let [t1, t2] = argTypes t0
      unifyM t1 (typeOf e1)
      unifyM t2 (typeOf e2)
      ty <- applySubstitution t0
      pure (eOp2 (ty, op) e1 e2)
    ECase _ [] -> throwError IllFormedExpression
    ECase expr clauses -> do
      e <- expr
      eCase e <$> inferCases e clauses
    ERow row -> eRow <$> inferRow row
    EField field expr1 expr2 -> do
      e1 <- expr1
      (f, e2) <- inferRowCase (typeOf e1) field expr2
      pure (eField f e1 e2)

type TypedClause = ([Label MonoType], TypedExpr)

inferRowCase ::
  MonoType -> [Label Int] -> TypeChecker TypedExpr -> TypeChecker TypedClause
inferRowCase (Fix (TRow row)) args expr = do
  case args of
    [(u0, label), (u1, v1), (u2, v2)] -> do
      let (r1, q) = restrictRow label row
      let [t0, t1, t2] = tVar <$> [u0, u1, u2]
      unifyM t1 r1
      unifyM t2 (tRow q)
      traverse applySubstitution [t0, t1, t2, t1 ~> t2 ~> tRow row]
        >>= \case
          [ty0, ty1, ty2, ty3] -> do
            unifyM ty0 ty3
            e <-
              local
                (Env.inserts [(label, promote ty0), (v1, promote ty1), (v2, promote ty2)])
                expr
            pure ([(t0, label), (t1, v1), (t2, v2)], e)

unopType :: Op1 -> PolyType
unopType =
  \case
    ONot -> tBool ~> tBool
    ONeg -> tGen "a0" ~> tGen "a0"

binopType :: Op2 -> PolyType
binopType =
  \case
    OEq -> tGen "a0" ~> tGen "a0" ~> tBool
    OAdd -> tGen "a0" ~> tGen "a0" ~> tGen "a0"
    OSub -> tGen "a0" ~> tGen "a0" ~> tGen "a0"
    OMul -> tGen "a0" ~> tGen "a0" ~> tGen "a0"
    ODiv -> tGen "a0" ~> tGen "a0" ~> tGen "a0"
    OLogicOr -> tBool ~> tBool ~> tBool
    OLogicAnd -> tBool ~> tBool ~> tBool

inferCases ::
  TypedExpr -> [([Label Int], TypeChecker TypedExpr)] -> TypeChecker [TypedClause]
inferCases expr clauses = do
  cs <- traverse inferClause clauses
  let t : ts = snd <$> cs
  forM_ ts (unifyM (typeOf t) . typeOf)
  pure cs
  where
    inferClause =
      secondM applySubstitution <=< uncurry (inferCase (typeOf expr))

inferCase ::
  MonoType -> [Label Int] -> TypeChecker TypedExpr -> TypeChecker TypedClause
inferCase mt (con : vs) expr = do
  (t, _) <- lookupName con ConstructorNotInScope
  let ts = unwindType t
      ps = (snd <$> vs) `zip` ts
  unifyM (typeOf mt) (last ts)
  e <- local (Env.inserts (promote <$$> ps)) expr
  tvs <-
    forM (zip vs ts) $ \((t0, n), t1) -> do
      unifyM (tVar t0) t1
      pure (tVar t0, n)
  pure ((t, snd con) : tvs, e)

inferRow ::
  Row TaggedExpr (Label Int) ->
  TypeChecker (Row TypedExpr (Label MonoType))
inferRow =
  cata $
    \case
      RNil -> pure rNil
      RVar var -> rVar <$> lookupName var NotInScope
      RExt name expr row -> rExt name <$> inferExpr expr <*> row

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

deriving instance (MonadReader TypeEnv) TypeChecker

deriving instance (MonadError TypeError) TypeChecker

deriving instance MonadFix TypeChecker

deriving instance Generic (TypeChecker a)

instance Newtype (TypeChecker a)

-- TypeError
deriving instance Show TypeError

deriving instance Eq TypeError
