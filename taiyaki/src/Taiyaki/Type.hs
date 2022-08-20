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
import Data.Either (fromRight)
import Data.Foldable (foldrM)
import Data.List (intersect)
import Data.Map ((!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import Taiyaki.Data
import Taiyaki.Data.Cons
import Taiyaki.Lang
import Taiyaki.Util
import qualified Taiyaki.Util.Env as Env

type SubMap = Map MonoIndex MonoType

newtype Substitution = Substitution SubMap

{-# INLINE domain #-}
domain :: Substitution -> [MonoIndex]
domain = Map.keys . unpack

mapsTo :: MonoIndex -> MonoType -> Substitution
mapsTo n = Substitution <<< Map.singleton n

compose :: Substitution -> Substitution -> Substitution
compose (Substitution s1) (Substitution s2) =
  Substitution (apply (Substitution s1) s2 `Map.union` s1)

merge :: Substitution -> Substitution -> Maybe Substitution
merge s1 s2
  | restr s1 == restr s2 = Just (over2 Substitution Map.union s1 s2)
  | otherwise = Nothing
  where
    dom = Set.fromList (domain s1 `intersect` domain s2)
    restr (Substitution s) = Map.restrictKeys s dom

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

instance (Substitutable t) => Substitutable (Predicate t) where
  apply sub =
    \case
      InClass n t             -> InClass n (apply sub t)

instance (Substitutable t) => Substitutable (Binding t) where
  apply sub =
    \case
      BPat t p                -> BPat (apply sub t) (apply sub p)
      BFun t n ps             -> BFun (apply sub t) n (apply sub ps)

instance (Substitutable a) => Substitutable (Choice a) where
  apply sub =
    \case
      Choice es e             -> Choice (apply sub es) (apply sub e)

instance
  ( Substitutable t
  , Substitutable a
  ) => Substitutable (CaseClause t a)
  where
  apply sub =
    \case
      Case t n ns e           -> Case (apply sub t) n ns (apply sub e)

{- ORMOLU_ENABLE -}

instance
  ( Substitutable t
  , Substitutable p
  , Substitutable a
  ) =>
  Substitutable (Clause t p a)
  where
  apply sub =
    \case
      Clause t p cs ->
        Clause (apply sub t) (apply sub p) (apply sub cs)

-------------------------------------------------------------------------------

data TypeError
  = UnificationError
  | InfiniteType
  | KindMismatch
  | CannotMerge
  | ContextReductionFailed

class HasIndex a where
  getIndex :: a -> MonoIndex
  updateIndex :: MonoIndex -> a -> a

instance HasIndex MonoIndex where
  getIndex = id
  updateIndex = const

unifyPairs ::
  (HasIndex s, MonadState s m, MonadError TypeError m) =>
  MonoType ->
  MonoType ->
  MonoType ->
  MonoType ->
  m Substitution
unifyPairs t1 u1 t2 u2 = do
  sub1 <- unifyTypes t1 t2
  sub2 <- unifyTypes (apply sub1 u1) (apply sub1 u2)
  pure (sub2 <> sub1)

unifyRows ::
  (HasIndex s, MonadState s m, MonadError TypeError m) =>
  MonoType ->
  MonoType ->
  m Substitution
unifyRows ty1 ty2 =
  case (unpackRow ty1, unpackRow ty2) of
    ((m1, Fix (TVar k r)), (m2, i))
      | Map.null m1 && not (Map.null m2) && i == tVar k r ->
          throwError UnificationError
      | Map.null m1 -> bindType (k, r) ty2
    ((m1, j), (m2, Fix (TVar k r)))
      | Map.null m2 && not (Map.null m1) && j == tVar k r ->
          throwError UnificationError
      | Map.null m2 -> bindType (k, r) ty1
    ((m1, j), (m2, i))
      | Map.null m1 -> unifyTypes ty1 ty2
      | otherwise ->
          case Map.lookup a m2 of
            Just (u : us) -> do
              let r1 = packRow (updateMap m1 ts, j)
                  r2 = packRow (updateMap m2 us, i)
              unifyPairs r1 t r2 u
            _
              | i == j -> throwError UnificationError
              | otherwise -> do
                  p <- tVar kRow <$> index
                  let r1 = packRow (updateMap m1 ts, j)
                      r2 = packRow (m2, p)
                  unifyPairs r1 i r2 (rExt a t p)
      where
        (a, t : ts) = Map.elemAt 0 m1
        updateMap m =
          \case
            [] -> Map.delete a m
            us -> Map.insert a us m

unifyTypes ::
  (HasIndex s, MonadState s m, MonadError TypeError m) =>
  MonoType ->
  MonoType ->
  m Substitution
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
      | otherwise -> unifyPairs t1 u1 t2 u2
    (TArr t1 u1, TArr t2 u2) ->
      unifyPairs t1 u1 t2 u2
    (TList t1, TList t2) ->
      unifyTypes t1 t2
    (TRec r1, TRec r2) ->
      unifyTypes r1 r2
    _
      | ty1 == ty2 -> pure mempty
      | otherwise -> throwError UnificationError

matchPairs ::
  (HasIndex s, MonadState s m, MonadError TypeError m) =>
  MonoType ->
  MonoType ->
  MonoType ->
  MonoType ->
  m Substitution
matchPairs t1 u1 t2 u2 = do
  sub1 <- matchTypes t1 t2
  sub2 <- matchTypes (apply sub1 u1) (apply sub1 u2)
  maybe (throwError CannotMerge) pure (merge sub1 sub2)

matchRows ::
  (HasIndex s, MonadState s m, MonadError TypeError m) =>
  MonoType ->
  MonoType ->
  m Substitution
matchRows ty1 ty2 =
  case (unpackRow ty1, unpackRow ty2) of
    ((m1, Fix (TVar k r)), (m2, i))
      | Map.null m1 && not (Map.null m2) && i == tVar k r ->
          throwError UnificationError
      | Map.null m1 -> bindType (k, r) ty2
    ((m1, j), (m2, i))
      | Map.null m1 -> matchTypes ty1 ty2
      | otherwise ->
          case Map.lookup a m2 of
            Just (u : us) -> do
              let r1 = packRow (updateMap m1 ts, j)
                  r2 = packRow (updateMap m2 us, i)
              matchPairs r1 t r2 u
            _
              | i == j -> throwError UnificationError
              | otherwise -> do
                  p <- tVar kRow <$> index
                  let r1 = packRow (updateMap m1 ts, j)
                      r2 = packRow (m2, p)
                  matchPairs r1 i r2 (rExt a t p)
      where
        (a, t : ts) = Map.elemAt 0 m1
        updateMap m =
          \case
            [] -> Map.delete a m
            us -> Map.insert a us m

matchTypes ::
  (HasIndex s, MonadState s m, MonadError TypeError m) =>
  MonoType ->
  MonoType ->
  m Substitution
matchTypes ty1 ty2 =
  case (project ty1, project ty2) of
    (TExt{}, _) ->
      matchRows ty1 ty2
    (_, TExt{}) ->
      matchRows ty1 ty2
    (TVar k n, _) ->
      bindType (k, n) ty2
    (TCon k1 c1, TCon k2 c2)
      | k1 /= k2 -> throwError KindMismatch
      | c1 == c2 && k1 == k2 -> pure mempty
    (TApp k1 t1 u1, TApp k2 t2 u2)
      | k1 /= k2 -> throwError KindMismatch
      | otherwise -> matchPairs t1 u1 t2 u2
    (TArr t1 u1, TArr t2 u2) ->
      matchPairs t1 u1 t2 u2
    (TList t1, TList t2) ->
      matchTypes t1 t2
    (TRec r1, TRec r2) ->
      matchTypes r1 r2
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

--------------------------------------------------------------------------------

super :: ClassEnv -> Name -> [Name]
super env name = maybe [] (classInfoSuperClasses . fst) (Env.lookup name env)

superPlus :: ClassEnv -> Name -> [Name]
superPlus env name = name : super env name

superClosure :: ClassEnv -> Name -> [Name]
superClosure env name =
  case super env name of
    [] -> [name]
    ns -> name : (superClosure env =<< ns)

instances :: ClassEnv -> Name -> [ClassInstance]
instances env name = maybe [] snd (Env.lookup name env)

-- bySuper :: ClassEnv t -> Predicate t -> [Predicate t]
bySuper env self@(InClass name t) =
  self : concat [bySuper env (InClass n t) | n <- super env name]

-- entail :: (Eq t) => ClassEnv t -> [Predicate t] -> Predicate t -> Bool
entail env ps p = any (p `elem`) (bySuper env <$> ps)

byInstance :: ClassEnv -> Predicate MonoType -> Maybe [Predicate MonoType]
byInstance env (InClass n s) = msum (tryInstance <$> instances env n)
  where
    tryInstance (ClassInstance ps t _) =
      case evalStateT (matchTypes t s) (freeIndex [t, s]) of
        Left{} -> Nothing
        Right sub -> Just (apply sub ps)

{- ORMOLU_DISABLE -}

isNormalForm :: Predicate (Type v) -> Bool
isNormalForm (InClass _ t) =
  (`cata` t)
    ( \case
        TApp _ t1 _ -> t1
        TVar{}      -> True
        _           -> False
    )

{- ORMOLU_ENABLE -}

toNormalForms ::
  (MonadError TypeError m) =>
  ClassEnv ->
  [Predicate MonoType] ->
  m [Predicate MonoType]
toNormalForms env ps = fmap concat (mapM hnf ps)
  where
    hnf tycl
      | isNormalForm tycl = pure [tycl]
      | otherwise =
          case byInstance env tycl of
            Nothing -> throwError ContextReductionFailed
            Just qs -> toNormalForms env qs

-- simplify :: (Eq t) => ClassEnv t -> [Predicate MonoType] -> [Predicate MonoType]
simplify env = go []
  where
    go qs [] = qs
    go qs (p : ps) = go (if entail env (qs <> ps) p then qs else p : qs) ps

reduce :: ClassEnv -> [Predicate MonoType] -> Either TypeError [Predicate MonoType]
reduce env ps = runExcept (simplify env <$> toNormalForms env ps)

reduceSet :: ClassEnv -> [Name] -> [Name]
reduceSet env names =
  reduce env [InClass name (tVar kTyp (MonoIndex 0)) | name <- names]
    & fromRight (error "Implementation error")
    <&> \(InClass n _) -> n

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
