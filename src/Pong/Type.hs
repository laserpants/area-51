{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Pong.Type where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Newtype.Generics (Newtype, over2, pack, unpack)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (fromList, toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Tuple.Extra (first, firstM, second, secondM)
import GHC.Generics (Generic)
import Pong.Data
import Pong.Lang
import Pong.Util
  ( Fix (..)
  , Name
  , Void
  , cata
  , embed
  , getAndModify
  , project
  , (!?)
  , (<$$>)
  , (<&>)
  , (<<<)
  )
import Pong.Util.Env (Environment)
import qualified Pong.Util.Env as Env

newtype Substitution
  = Substitution (Map Int MonoType)

type TypeEnv = Environment (Either MonoType Scheme)

newtype TypeChecker a
  = TypeChecker
      (ExceptT TypeError (ReaderT TypeEnv (State (Int, Substitution))) a)

data TypeError
  = UnificationError
  | InfiniteType
  | NotInScope Name
  | ConstructorNotInScope Name
  | IllFormedExpression

evalTypeChecker :: Int -> TypeEnv -> TypeChecker a -> Either TypeError a
evalTypeChecker n env (TypeChecker a) =
  evalState (runReaderT (runExceptT a) env) (n, mempty)

runTypeChecker ::
  Int ->
  TypeEnv ->
  TypeChecker a ->
  (Either TypeError a, (Int, Substitution))
runTypeChecker n env (TypeChecker c) =
  runState (runReaderT (runExceptT c) env) (n, mempty)

moduleEnv :: ModuleDefs t a -> TypeEnv
moduleEnv = Env.fromList . concatMap go . Map.toList
  where
    go = \case
      ((Scheme s, _), Data _ cons) ->
        cons
          <&> ( \(Fix (TCon con fs)) ->
                  (con, Right (Scheme (foldType s fs)))
              )
      ((scheme, defn), _) ->
        [ (defn, Right scheme)
        ]

-------------------------------------------------------------------------------
-- Substitution
-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

substitute :: Map Int MonoType -> MonoType -> MonoType
substitute sub =
 cata
   ( \case
       TVar n       -> fromMaybe (tVar n) (sub !? n)
       TCon c ts    -> tCon c ts
       TArr t1 t2   -> tArr t1 t2
       TRec row     -> tRec row
       TUnit        -> tUnit
       TBool        -> tBool
       TInt         -> tInt
       TFloat       -> tFloat
       TDouble      -> tDouble
       TChar        -> tChar
       TString      -> tString
       RNil         -> rNil
       RExt n t1 t2 -> rExt n t1 t2
   )

class Substitutable a where
  apply :: Substitution -> a -> a

instance Substitutable () where
  apply _ = const ()

instance Substitutable MonoType where
  apply = substitute . unpack

instance (Substitutable a) => Substitutable [a] where
  apply = fmap . apply

instance (Substitutable a) => Substitutable (Map k a) where
  apply = fmap . apply

{- ORMOLU_DISABLE -}

instance
  (Substitutable a0, Substitutable a2, Substitutable t) =>
  Substitutable (Expr t a0 a1 a2)
  where
  apply sub =
    cata
    ( \case
        EVar name                -> eVar (applyFst name)
        ECon con                 -> eCon (first (apply sub) con)
        ELet bind expr1 expr2    -> eLet (applyFst bind) expr1 expr2
        EApp t fun args          -> eApp (apply sub t) fun args
        ELam t args expr         -> eLam t (applyFst <$> args) expr
        ECall t fun args         -> eCall (apply sub t) (applyFst fun) args
        EOp1 (t, op) expr1       -> eOp1 (apply sub t, op) expr1
        EOp2 (t, op) expr1 expr2 -> eOp2 (apply sub t, op) expr1 expr2
        EPat expr cs             -> ePat expr (first (fmap applyFst) <$> cs)
        ERes field expr1 expr2   -> eRes (applyFst <$> field) expr1 expr2
        e                        -> embed e
      )
    where
      applyFst = first (apply sub)

{- ORMOLU_ENABLE -}

instance Substitutable Void where
  apply = const id

instance
  (Substitutable t, Substitutable a) =>
  Substitutable (Definition t a)
  where
  apply sub =
    \case
      Function as (t, a) ->
        Function (first (apply sub) <$> as) (apply sub t, apply sub a)
      Constant (t, a) ->
        Constant (apply sub t, apply sub a)
      Extern ts t ->
        Extern (apply sub ts) (apply sub t)
      def ->
        def

instance (Substitutable t, Substitutable a) => Substitutable (Module t a) where
  apply = fmap . apply

compose :: Substitution -> Substitution -> Substitution
compose = over2 Substitution fun
  where
    fun s1 s2 = apply (Substitution s1) s2 `Map.union` s1

mapsTo :: Int -> MonoType -> Substitution
mapsTo n = pack <<< Map.singleton n

-------------------------------------------------------------------------------
-- Tagging
-------------------------------------------------------------------------------

tagFst :: a -> TypeChecker (Int, a)
tagFst a = do
  t <- tag
  pure (t, a)

tagExpr :: SourceExpr -> TypeChecker TaggedExpr
tagExpr =
  cata
    ( \case
        EVar (_, name) ->
          eVar <$> tagFst name
        ECon (_, con) ->
          eCon <$> tagFst con
        ELit prim ->
          pure (eLit prim)
        EIf e1 e2 e3 ->
          eIf <$> e1 <*> e2 <*> e3
        ELet (_, name) e1 e2 ->
          eLet <$> tagFst name <*> e1 <*> e2
        EApp _ fun args ->
          eApp <$> tag <*> fun <*> sequence args
        ELam _ args expr ->
          eLam () <$> traverse (tagFst . snd) args <*> expr
        EOp1 (_, op) e1 ->
          eOp1 <$> tagFst op <*> e1
        EOp2 (_, op) e1 e2 ->
          eOp2 <$> tagFst op <*> e1 <*> e2
        EPat e1 cs ->
          ePat
            <$> e1
            <*> traverse (firstM (traverse (tagFst . snd)) <=< sequence) cs
        ENil ->
          pure eNil
        EExt name e1 e2 ->
          eExt name <$> e1 <*> e2
        ERes f e1 e2 ->
          eRes <$> traverse (tagFst . snd) f <*> e1 <*> e2
    )

tag :: MonadState (Int, a) m => m Int
tag = fst <$> getAndModify (first succ)

-------------------------------------------------------------------------------
-- Unification
-------------------------------------------------------------------------------

unifyAndCombine ::
  MonoType -> MonoType -> Substitution -> TypeChecker Substitution
unifyAndCombine t1 t2 sub1 = do
  sub2 <- unifyTypes (apply sub1 t1) (apply sub1 t2)
  pure (sub2 <> sub1)

unifyMany :: [MonoType] -> [MonoType] -> TypeChecker Substitution
unifyMany ts us = foldrM (uncurry unifyAndCombine) mempty (ts `zip` us)

unifyRows :: MonoType -> MonoType -> TypeChecker Substitution
unifyRows ty1 ty2 =
  case (unwindRow ty1, unwindRow ty2) of
    ((m1, Fix (TVar r)), (m2, k))
      | Map.null m1 && not (Map.null m2) && k == tVar r ->
          throwError UnificationError
      | Map.null m1 -> bindType r ty2
    ((m1, j), (m2, Fix (TVar r)))
      | Map.null m2 && not (Map.null m1) && j == tVar r ->
          throwError UnificationError
      | Map.null m2 -> bindType r ty1
    ((m1, j), (m2, k))
      | Map.null m1 -> unifyTypes ty1 ty2
      | otherwise ->
          case Map.lookup a m2 of
            Just (u : us) -> do
              let r1 = foldRow j (updateMap m1 ts)
                  r2 = foldRow k (updateMap m2 us)
              unifyMany [r1, t] [r2, u]
            _
              | k == j -> throwError UnificationError
              | otherwise -> do
                  p <- tVar <$> tag
                  let r1 = foldRow j (updateMap m1 ts)
                      r2 = foldRow p m2
                  unifyMany [r1, k] [r2, rExt a t p]
      where
        (a, t : ts) = Map.elemAt 0 m1
        updateMap m =
          \case
            [] -> Map.delete a m
            us -> Map.insert a us m

unifyTypes :: MonoType -> MonoType -> TypeChecker Substitution
unifyTypes ty1 ty2 =
  case (project ty1, project ty2) of
    (RExt{}, _) ->
      unifyRows ty1 ty2
    (_, RExt{}) ->
      unifyRows ty1 ty2
    (TVar n, _) ->
      bindType n ty2
    (_, TVar n) ->
      bindType n ty1
    (TCon c1 ts1, TCon c2 ts2)
      | c1 == c2 ->
          unifyMany ts1 ts2
    (TArr t1 t2, TArr u1 u2) ->
      unifyMany [t1, t2] [u1, u2]
    (TRec r1, TRec r2) ->
      unifyTypes r1 r2
    _
      | ty1 == ty2 -> pure mempty
      | otherwise -> throwError UnificationError

applySubstitution :: (Substitutable s) => s -> TypeChecker s
applySubstitution s = gets (apply . snd) <*> pure s

unify :: MonoType -> MonoType -> TypeChecker ()
unify t1 t2 = do
  sub <- gets snd
  sub1 <- unifyTypes (apply sub t1) (apply sub t2)
  modify (second (sub1 <>))

instantiate :: Scheme -> TypeChecker MonoType
instantiate (Scheme t) = do
  ts <- traverse (\n -> tag >>= \v -> pure (n, v)) (Set.toList (boundVars t))
  pure (toMonoType (Map.fromList ts) t)

generalize :: MonoType -> TypeChecker Scheme
generalize t = do
  env <- ask
  t1 <- applySubstitution t
  let vars = filter (`notElem` free env) (free t1)
  pure (toScheme "a" vars t1)

{- ORMOLU_DISABLE -}

bindType :: Int -> MonoType -> TypeChecker Substitution
bindType n ty
  | tVar n == ty     = pure mempty
  | n `elem` free ty = throwError InfiniteType
  | otherwise        = pure (n `mapsTo` ty)

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------
-- Type inference
-------------------------------------------------------------------------------

lookupName :: Label Int -> (Name -> TypeError) -> TypeChecker (MonoType, Name)
lookupName (t, var) toErr = do
  ty <- getTy
  tVar t `unify` ty
  pure (ty, var)
  where
    getTy =
      asks (Env.lookup var)
        >>= \case
          Just (Right s) -> instantiate s
          Just (Left ty) -> pure ty
          _ -> throwError (toErr var)

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
      typeOf e1 `unify` tBool
      typeOf e2 `unify` typeOf e3
      pure (eIf e1 e2 e3)
    ELet (t, var) expr1 expr2 -> do
      fv <- tVar <$> tag
      e1 <- local (Env.insert var (Left fv)) expr1
      s <- generalize (typeOf e1)
      e2 <- local (Env.insert var (Right s)) expr2
      tVar t `unify` typeOf e1
      t0 <- applySubstitution (tVar t)
      pure (eLet (t0, var) e1 e2)
    EApp t fun args -> do
      f <- fun
      as <- sequence args
      t1 <- applySubstitution (tVar t)
      typeOf f `unify` foldType t1 (typeOf <$> as)
      pure (eApp t1 f as)
    ELam _ args expr -> do
      as <- traverse (pure . first tVar) args
      e <- local (insertArgs (first Left <$> as)) expr
      pure (eLam () as e)
    EOp1 (_, op) expr1 -> do
      e1 <- expr1
      t0 <- instantiate (unopType op)
      let [t1] = argTypes t0
      t1 `unify` typeOf e1
      ty <- applySubstitution t0
      pure (eOp1 (ty, op) e1)
    EOp2 (_, op) expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      t0 <- instantiate (binopType op)
      let [t1, t2] = argTypes t0
      t1 `unify` typeOf e1
      t2 `unify` typeOf e2
      ty <- applySubstitution t0
      pure (eOp2 (ty, op) e1 e2)
    EPat _ [] -> throwError IllFormedExpression
    EPat expr clauses -> do
      e <- expr
      ePat e <$> inferPat e clauses
    ENil ->
      pure eNil
    EExt name expr1 expr2 -> do
      e1 <- expr1
      e2 <- expr2
      eExt name e1
        <$> case project e2 of
          EVar (t, var) -> do
            t0 <- tRec . tVar <$> tag
            t0 `unify` t
            pure (eVar (t0, var))
          _ ->
            pure e2
    ERes field expr1 expr2 -> do
      e1 <- expr1
      (f, e2) <- inferRestriction (typeOf e1) field expr2
      pure (eRes f e1 e2)

inferRestriction ::
  MonoType ->
  [Label Int] ->
  TypeChecker TypedExpr ->
  TypeChecker (Clause MonoType TypedExpr)
inferRestriction (Fix (TRec row)) [(u0, label), (u1, v1), (u2, v2)] expr = do
  let (r1, q) = restrictRow label row
  let [t0, t1, t2] = tVar <$> [u0, u1, u2]
  t1 `unify` r1
  t2 `unify` tRec q
  traverse applySubstitution [t0, t1, t2, t1 ~> t2 ~> tRec row]
    >>= \case
      [ty0, ty1, ty2, ty3] -> do
        ty0 `unify` ty3
        e <-
          local
            (Env.inserts [(label, Left ty0), (v1, Left ty1), (v2, Left ty2)])
            expr
        pure ([(t0, label), (t1, v1), (t2, v2)], e)
      _ ->
        error "Implementation error"
inferRestriction _ _ _ =
  error "Implementation error"

unopType :: Op1 -> Scheme
unopType =
  Scheme
    <<< \case
      ONot -> tBool ~> tBool
      ONeg -> tVar "a" ~> tVar "a"

{- ORMOLU_DISABLE -}

binopType :: Op2 -> Scheme
binopType =
 Scheme
   <<< \case
     OEq  -> tVar "a" ~> tVar "a" ~> tBool
     ONEq -> tVar "a" ~> tVar "a" ~> tBool
     OLt  -> tVar "a" ~> tVar "a" ~> tBool
     OGt  -> tVar "a" ~> tVar "a" ~> tBool
     OLtE -> tVar "a" ~> tVar "a" ~> tBool
     OGtE -> tVar "a" ~> tVar "a" ~> tBool
     OAdd -> tVar "a" ~> tVar "a" ~> tVar "a"
     OSub -> tVar "a" ~> tVar "a" ~> tVar "a"
     OMul -> tVar "a" ~> tVar "a" ~> tVar "a"
     ODiv -> tVar "a" ~> tVar "a" ~> tVar "a"
     OLogicOr  -> tBool ~> tBool ~> tBool
     OLogicAnd -> tBool ~> tBool ~> tBool

{- ORMOLU_ENABLE -}

inferPat ::
  TypedExpr ->
  [Clause Int (TypeChecker TypedExpr)] ->
  TypeChecker [Clause MonoType TypedExpr]
inferPat expr clauses = do
  cs <- traverse inferClause clauses
  let t : ts = snd <$> cs
  forM_ ts (unify (typeOf t) . typeOf)
  pure cs
  where
    inferClause =
      secondM applySubstitution <=< uncurry (inferCase (typeOf expr))

inferCase ::
  MonoType ->
  [Label Int] ->
  TypeChecker TypedExpr ->
  TypeChecker (Clause MonoType TypedExpr)
inferCase mt (con : vs) expr = do
  (t, _) <- lookupName con ConstructorNotInScope
  let ts = unwindType t
      ps = (snd <$> vs) `zip` ts
  typeOf mt `unify` last ts
  e <- local (Env.inserts (Left <$$> ps)) expr
  tvs <-
    forM (zip vs ts) $ \((t0, n), t1) -> do
      tVar t0 `unify` t1
      pure (tVar t0, n)
  pure ((t, snd con) : tvs, e)
inferCase _ _ _ = error "Implementation error"

inferModuleDefs ::
  ModuleDefs () SourceExpr -> TypeChecker (ModuleDefs MonoType TypedExpr)
inferModuleDefs p = local (<> moduleEnv p) (moduleForM p (curry go))
  where
    inferTypes =
      tagExpr >=> inferExpr >=> applySubstitution
    go =
      \case
        ((scheme, _), Function args (_, expr)) -> do
          t0 <- instantiate scheme
          let as_ = argTypes t0 `zip` (snd <$> toList args)
              localEnv = local (insertArgs (first Left <$> as_))
          lam <- localEnv (eLam () as_ <$> inferTypes expr)
          t0 `unify` typeOf lam
          applySubstitution lam <&> project >>= \case
            ELam () as body ->
              pure (Function (fromList as) (typeOf body, body))
            _ ->
              error "Implementation error"
        (_, Constant (_, expr)) -> do
          e <- inferTypes expr
          pure (Constant (typeOf e, e))
        (_, Extern as r) -> do
          pure (Extern as r)
        (_, Data name cons) ->
          pure (Data name cons)

runInferModuleDefs ::
  ModuleDefs () SourceExpr ->
  Either TypeError (ModuleDefs MonoType TypedExpr)
runInferModuleDefs = runTypeChecker 1 mempty . inferModuleDefs <&> fst

runInferModule ::
  Module () SourceExpr ->
  Either TypeError (Module MonoType TypedExpr)
runInferModule (Module n p) = Module n <$> runInferModuleDefs p

-------------------------------------------------------------------------------
-- Typeclass instances
-------------------------------------------------------------------------------

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
