{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Taiyaki.Type where

import Control.Monad.Except
import Control.Monad.State
import Control.Newtype.Generics (pack, unpack)
import Data.Foldable (foldrM)
import Data.Map ((!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Taiyaki.Data
import Taiyaki.Data.Cons
import Taiyaki.Lang
import Taiyaki.Util

type SubMap = Map MonoIndex MonoType

newtype Substitution = Substitution SubMap

compose :: Substitution -> Substitution -> Substitution
compose = over2 Substitution fun
  where
    fun s1 s2 = apply (Substitution s1) s2 `Map.union` s1

mapsTo :: MonoIndex -> MonoType -> Substitution
mapsTo n = pack <<< Map.singleton n

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

substitute :: SubMap -> MonoType -> MonoType
substitute sub =
  cata
    ( \case
        TVar k n         -> fromMaybe (tVar k n) (sub !? n)
        TUnit            -> tUnit
        TBool            -> tBool
        TInt             -> tInt
        TBig             -> tBig
        TNat             -> tNat
        TFloat           -> tFloat
        TDouble          -> tDouble
        TChar            -> tChar
        TString          -> tString
        TVoid            -> tVoid
        TList t          -> tList t
        TCon k n         -> tCon k n
        TApp k t1 t2     -> tApp k t1 t2
        TArr t1 t2       -> tArr t1 t2
        TRec t           -> tRec t
        TNil             -> tNil
        TExt n t1 t2     -> tExt n t1 t2
    )

{- ORMOLU_ENABLE -}

class Substitutable a where
  apply :: Substitution -> a -> a

instance Substitutable MonoType where
  apply = substitute . unpack

instance (Substitutable a) => Substitutable [a] where
  apply = fmap . apply

instance (Substitutable a) => Substitutable (Map k a) where
  apply = fmap . apply

instance Substitutable Void where
  apply = const id

{- ORMOLU_DISABLE -}

instance
  (Substitutable t) =>
  Substitutable (Op1 t)
  where
  apply sub =
    \case
      ONot t   -> ONot (apply sub t)
      ONeg t   -> ONeg (apply sub t)

instance
  (Substitutable t) =>
  Substitutable (Op2 t)
  where
  apply sub =
    \case
      OEq   t  -> OEq (apply sub t)
      ONEq  t  -> ONEq (apply sub t)
      OLt   t  -> OLt (apply sub t)
      OGt   t  -> OGt (apply sub t)
      OLtE  t  -> OLtE (apply sub t)
      OGtE  t  -> OGtE (apply sub t)
      OAdd  t  -> OAdd (apply sub t)
      OSub  t  -> OSub (apply sub t)
      OMul  t  -> OMul (apply sub t)
      ODiv  t  -> ODiv (apply sub t)
      OPow  t  -> OPow (apply sub t)
      OMod  t  -> OMod (apply sub t)
      OOr   t  -> OOr (apply sub t)
      OAnd  t  -> OAnd (apply sub t)
      OLArr t  -> OLArr (apply sub t)
      ORarr t  -> ORarr (apply sub t)
      OFPip t  -> OFPip (apply sub t)
      OBPip t  -> OBPip (apply sub t)
      ODot  t  -> ODot (apply sub t)
      OGet  t  -> OGet (apply sub t)

instance
  (Substitutable t) =>
  Substitutable (Pattern t)
  where
  apply sub =
    cata
      ( \case
          PVar  t a1          -> pVar (apply sub t) a1
          PLit  t a1          -> pLit (apply sub t) a1
          PAs   t a1 a2       -> pAs (apply sub t) a1 a2
          POr   t a1 a2       -> pOr (apply sub t) a1 a2
          PAny  t             -> pAny (apply sub t)
          PCon  t a1 a2       -> pCon (apply sub t) a1 a2
          PTup  t a1          -> pTup (apply sub t) a1
          PList t a1          -> pList (apply sub t) a1
          PRec  t a1          -> pRec (apply sub t) a1
          PNil  t             -> pNil (apply sub t)
          PExt  t a1 a2 a3    -> pExt (apply sub t) a1 a2 a3
          PAnn  t a1          -> pAnn (apply sub t) a1
      )

instance
  ( Functor e2
  , Functor e3
  , Substitutable t
  , Substitutable e1
  , Substitutable (e2 (Expr t e1 e2 e3 e4))
  , Substitutable (e3 (Expr t e1 e2 e3 e4))
  , Substitutable e4
  ) =>
  Substitutable (Expr t e1 e2 e3 e4)
  where
  apply sub =
    cata
      ( \case
          EVar  t a1          -> eVar (apply sub t) a1
          ECon  t a1          -> eCon (apply sub t) a1
          ELit  t a1          -> eLit (apply sub t) a1
          EApp  t a1 a2       -> eApp (apply sub t) a1 a2
          ELam  t a1 a2       -> eLam (apply sub t) (apply sub a1) a2
          EIf   t a1 a2 a3    -> eIf (apply sub t) a1 a2 a3
          EPat  t a1 a2       -> ePat (apply sub t) a1 (apply sub a2)
          ELet  t a1 a2 a3    -> eLet (apply sub t) (apply sub a1) a2 a3
          EFix  t a1 a2 a3    -> eFix (apply sub t) a1 a2 a3
          EFun  t a1          -> eFun (apply sub t) (apply sub a1)
          EOp1  t a1 a2       -> eOp1 (apply sub t) (apply sub a1) a2
          EOp2  t a1 a2 a3    -> eOp2 (apply sub t) (apply sub a1) a2 a3
          ETup  t a1          -> eTup (apply sub t) a1
          EList t a1          -> eList (apply sub t) a1
          ERec  t a1          -> eRec (apply sub t) a1
          ENil  t             -> eNil (apply sub t)
          EExt  t a1 a2 a3    -> eExt (apply sub t) a1 a2 a3
          ESub  t             -> eSub (apply sub t)
          ECo   t a1          -> eCo (apply sub t) a1
          EAnn  t a1          -> eAnn (apply sub t) a1
      )

{- ORMOLU_ENABLE -}

instance (Substitutable t) => Substitutable (Predicate t) where
  apply sub =
    \case
      InClass n t -> InClass n (apply sub t)

instance (Substitutable t) => Substitutable (Binding t) where
  apply sub =
    \case
      BPat t p ->
        BPat (apply sub t) (apply sub p)
      BFun t n ps ->
        BFun (apply sub t) n (apply sub ps)

instance (Substitutable a) => Substitutable (Choice a) where
  apply sub =
    undefined -- TODO

instance (Substitutable a) => Substitutable (Clause t p a) where
  apply sub =
    undefined -- TODO

instance (Substitutable t) => Substitutable (CaseClause t a) where
  apply sub =
    undefined -- TODO

-------------------------------------------------------------------------------

data TypeError
  = UnificationError
  | InfiniteType
  | KindMismatch

class HasIndex a where
  getIndex :: a -> MonoIndex
  updateIndex :: MonoIndex -> a -> a

instance HasIndex MonoIndex where
  getIndex = id
  updateIndex = const

unifyAndCombine ::
  (HasIndex s, MonadState s m, MonadError TypeError m) =>
  MonoType ->
  MonoType ->
  Substitution ->
  m Substitution
unifyAndCombine t1 t2 sub1 = do
  sub2 <- unifyTypes (apply sub1 t1) (apply sub1 t2)
  pure (sub2 <> sub1)

unifyMany :: (HasIndex s, MonadState s m, MonadError TypeError m) => [MonoType] -> [MonoType] -> m Substitution
unifyMany ts us = foldrM (uncurry unifyAndCombine) mempty (ts `zip` us)

unifyRows :: (HasIndex s, MonadState s m, MonadError TypeError m) => MonoType -> MonoType -> m Substitution
unifyRows ty1 ty2 =
  case (unpackRow ty1, unpackRow ty2) of
    ((m1, Fix (TVar w r)), (m2, k))
      | Map.null m1 && not (Map.null m2) && k == tVar w r ->
          throwError UnificationError
      | Map.null m1 -> bindType (w, r) ty2
    ((m1, j), (m2, Fix (TVar w r)))
      | Map.null m2 && not (Map.null m1) && j == tVar w r ->
          throwError UnificationError
      | Map.null m2 -> bindType (w, r) ty1
    ((m1, j), (m2, k))
      | Map.null m1 -> unifyTypes ty1 ty2
      | otherwise ->
          case Map.lookup a m2 of
            Just (u : us) -> do
              let r1 = packRow (updateMap m1 ts, j)
                  r2 = packRow (updateMap m2 us, k)
              unifyMany [r1, t] [r2, u]
            _
              | k == j -> throwError UnificationError
              | otherwise -> do
                  p <- tVar kRow <$> index
                  let r1 = packRow (updateMap m1 ts, j)
                      r2 = packRow (m2, p)
                  unifyMany [r1, k] [r2, rExt a t p]
      where
        (a, t : ts) = Map.elemAt 0 m1
        updateMap m =
          \case
            [] -> Map.delete a m
            us -> Map.insert a us m

unifyTypes :: (HasIndex s, MonadState s m, MonadError TypeError m) => MonoType -> MonoType -> m Substitution
unifyTypes ty1 ty2 =
  case (project ty1, project ty2) of
    (TExt{}, _) ->
      unifyRows ty1 ty2
    (_, TExt{}) ->
      unifyRows ty1 ty2
    (TVar k n, _) ->
      bindType (k, n) ty2
    (_, TVar k n) ->
      bindType (k, n) ty1
    (TCon k1 c1, TCon k2 c2)
      | k1 /= k2 -> throwError KindMismatch
      | c1 == c2 && k1 == k2 -> pure mempty
    (TApp k1 t1 u1, TApp k2 t2 u2)
      | k1 /= k2 -> throwError KindMismatch
      | otherwise -> unifyMany [t1, u1] [t2, u2]
    (TArr t1 u1, TArr t2 u2) ->
      unifyMany [t1, u1] [t2, u2]
    (TList t1, TList t2) ->
      unifyTypes t1 t2
    (TRec r1, TRec r2) ->
      unifyTypes r1 r2
    _
      | ty1 == ty2 -> pure mempty
      | otherwise -> throwError UnificationError

{- ORMOLU_DISABLE -}

bindType :: (MonadError TypeError m) => TyVar -> MonoType -> m Substitution
bindType tv@(k, n) ty
  | tVar k n == ty      = pure mempty
  | tv `elem` free ty   = throwError InfiniteType
  | k /= kindOf ty      = throwError KindMismatch
  | otherwise           = pure (n `mapsTo` ty)

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

index :: (HasIndex s, MonadState s m) => m MonoIndex
index = do
  a <- get
  let ix = getIndex a
  put (updateIndex (succ ix) a)
  pure ix

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

-- TypeError
deriving instance Show TypeError

deriving instance Eq TypeError

deriving instance Ord TypeError
