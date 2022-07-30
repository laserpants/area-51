{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Taiyaki.Lang where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set.Monad as Set
import qualified Data.Text as Text
import Taiyaki.Data
import Taiyaki.Data.Cons
import Taiyaki.Util
import qualified Taiyaki.Util.Env as Env

-------------------------------------------------------------------------------

class Row r where
  rNil :: r
  rExt :: Name -> r -> r -> r
  rInit :: r -> [(Name, r)]
  rLast :: r -> r

instance Row (Type v) where
  rNil = tNil
  rExt = tExt
  rInit =
    para
      ( \case
          TExt n p q ->
            (n, fst p) : snd q
          _ ->
            []
      )
  rLast =
    cata
      ( \case
          TExt _ _ r ->
            r
          r ->
            embed r
      )

instance (Row t) => Row (Expr t) where
  rNil = eNil rNil
  rExt n p q = eExt (rExt n (getTag p) (getTag q)) n p q
  rInit =
    para
      ( \case
          EExt _ n p q ->
            (n, fst p) : snd q
          _ ->
            []
      )
  rLast =
    cata
      ( \case
          EExt _ _ _ r ->
            r
          r ->
            embed r
      )

instance (Row t) => Row (Pattern t) where
  rNil = pNil rNil
  rExt n p q = pExt (rExt n (getTag p) (getTag q)) n p q
  rInit =
    para
      ( \case
          PExt _ n p q ->
            (n, fst p) : snd q
          _ ->
            []
      )
  rLast =
    cata
      ( \case
          PExt _ _ _ r ->
            r
          r ->
            embed r
      )

instance Row () where
  rNil = ()
  rExt _ _ _ = ()
  rInit _ = []
  rLast _ = ()

-------------------------------------------------------------------------------

class Tuple a t | a -> t where
  tup :: t -> [a] -> a

instance Tuple () () where
  tup _ _ = ()

instance Tuple (Type v) () where
  tup () ts = tApps (tCon (kFun n) (tupleCon n)) ts where n = length ts

instance Tuple (Expr t) t where
  tup = eTup

instance Tuple (Pattern t) t where
  tup = pTup

-------------------------------------------------------------------------------

class Con a t | a -> t where
  con :: t -> Name -> [a] -> a

instance Con (Type v) Kind where
  con k n ts = tApps (tCon (foldr kArr k (kindOf <$> ts)) n) ts

instance Con (Expr (Type v)) (Type v) where
  con t n [] = eCon t n
  con t n es = eApp t (eCon (foldr (tArr . getTag) t es) n) es

instance Con (Pattern t) t where
  con = pCon

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

type family Tag a t where
  Tag (Pattern _) t = Pattern t
  Tag (Binding _) t = Binding t
  Tag (Clause _)  t = Clause t
  Tag (Op1 _)     t = Op1 t
  Tag (Op2 _)     t = Op2 t
  Tag (Expr _)    t = Expr t

class Tagged a t | a -> t where
  getTag :: a -> t
  mapTag :: (Tag a t ~ a) => (t -> u) -> a -> Tag a u

instance Tagged (Pattern t) t where
  getTag =
    cata
      ( \case
          PVar  t _          -> t
          PLit  t _          -> t
          PAs   t _ _        -> t
          POr   t _ _        -> t
          PAny  t            -> t
          PCon  t _ _        -> t
          PTup  t _          -> t
          PList t _          -> t
          PRec  t _          -> t
          PNil  t            -> t
          PExt  t _ _ _      -> t
          PAnn  t _          -> t
      )

  mapTag f =
    cata
      ( \case
          PVar  t a1         -> pVar  (f t) a1
          PLit  t a1         -> pLit  (f t) a1
          PAs   t a1 a2      -> pAs   (f t) a1 a2
          POr   t a1 a2      -> pOr   (f t) a1 a2
          PAny  t            -> pAny  (f t)
          PCon  t a1 a2      -> pCon  (f t) a1 a2
          PTup  t a1         -> pTup  (f t) a1
          PList t a1         -> pList (f t) a1
          PRec  t a1         -> pRec  (f t) a1
          PNil  t            -> pNil  (f t)
          PExt  t a1 a2 a3   -> pExt  (f t) a1 a2 a3
          PAnn  t a1         -> pAnn  (f t) a1
      )

instance Tagged (Binding t) t where
  getTag =
    \case
      BPat t _               -> t
      BFun t _ _             -> t

  mapTag f =
    \case
      BPat t a1              -> BPat (f t) (mapTag f a1)
      BFun t a1 a2           -> BFun (f t) a1 (mapTag f <$> a2)

instance Tagged (Clause t) t where
  getTag =
    \case
      Clause t _ _           -> t

  mapTag f =
    \case
      Clause t a1 a2         -> Clause (f t) (mapTag f <$> a1) a2

instance Tagged (Op1 t) t where
  getTag =
    \case
      ONot t                 -> t
      ONeg t                 -> t

  mapTag f =
    \case
      ONot t                 -> ONot (f t)
      ONeg t                 -> ONeg (f t)

instance Tagged (Op2 t) t where
  getTag =
    \case
      OEq   t                -> t
      ONEq  t                -> t
      OLt   t                -> t
      OGt   t                -> t
      OLtE  t                -> t
      OGtE  t                -> t
      OAdd  t                -> t
      OSub  t                -> t
      OMul  t                -> t
      ODiv  t                -> t
      OPow  t                -> t
      OMod  t                -> t
      OOr   t                -> t
      OAnd  t                -> t
      OLArr t                -> t
      ORarr t                -> t
      OFPip t                -> t
      OBPip t                -> t
      ODot  t                -> t
      OGet  t                -> t

  mapTag f =
    \case
      OEq   t                -> OEq   (f t)
      ONEq  t                -> ONEq  (f t)
      OLt   t                -> OLt   (f t)
      OGt   t                -> OGt   (f t)
      OLtE  t                -> OLtE  (f t)
      OGtE  t                -> OGtE  (f t)
      OAdd  t                -> OAdd  (f t)
      OSub  t                -> OSub  (f t)
      OMul  t                -> OMul  (f t)
      ODiv  t                -> ODiv  (f t)
      OPow  t                -> OPow  (f t)
      OMod  t                -> OMod  (f t)
      OOr   t                -> OOr   (f t)
      OAnd  t                -> OAnd  (f t)
      OLArr t                -> OLArr (f t)
      ORarr t                -> ORarr (f t)
      OFPip t                -> OFPip (f t)
      OBPip t                -> OBPip (f t)
      ODot  t                -> ODot  (f t)
      OGet  t                -> OGet  (f t)

instance Tagged (Expr t) t where
  getTag =
    cata
      ( \case
          EVar  t _          -> t
          ECon  t _          -> t
          ELit  t _          -> t
          EApp  t _ _        -> t
          ELam  t _ _        -> t
          EIf   t _ _ _      -> t
          EPat  t _ _        -> t
          ELet  t _ _ _      -> t
          EFix  t _ _ _      -> t
          EFun  t _          -> t
          EOp1  t _ _        -> t
          EOp2  t _ _ _      -> t
          ETup  t _          -> t
          EList t _          -> t
          ERec  t _          -> t
          ENil  t            -> t
          EExt  t _ _ _      -> t
          ESub  t            -> t
          ECo   t _          -> t
          EAnn  t _          -> t
      )

  mapTag f =
    cata
      ( \case
          EVar  t a1         -> eVar  (f t) a1
          ECon  t a1         -> eCon  (f t) a1
          ELit  t a1         -> eLit  (f t) a1
          EApp  t a1 a2      -> eApp  (f t) a1 a2
          ELam  t a1 a2      -> eLam  (f t) (mapTag f <$> a1) a2
          EIf   t a1 a2 a3   -> eIf   (f t) a1 a2 a3
          EPat  t a1 a2      -> ePat  (f t) a1 (mapTag f <$> a2)
          ELet  t a1 a2 a3   -> eLet  (f t) (mapTag f a1) a2 a3
          EFix  t a1 a2 a3   -> eFix  (f t) a1 a2 a3
          EFun  t a1         -> eFun  (f t) (mapTag f <$> a1)
          EOp1  t a1 a2      -> eOp1  (f t) (mapTag f a1) a2
          EOp2  t a1 a2 a3   -> eOp2  (f t) (mapTag f a1) a2 a3
          ETup  t a1         -> eTup  (f t) a1
          EList t a1         -> eList (f t) a1
          ERec  t a1         -> eRec  (f t) a1
          ENil  t            -> eNil  (f t)
          EExt  t a1 a2 a3   -> eExt  (f t) a1 a2 a3
          ESub  t            -> eSub  (f t)
          ECo   t a1         -> eCo   (f t) a1
          EAnn  t a1         -> eAnn  (f t) a1
      )

setTag :: (Tagged a t, a ~ Tag a t) => t -> a -> a
setTag = mapTag . const

-------------------------------------------------------------------------------

kindOf :: Type v -> Kind
kindOf =
  cata
    ( \case
        TVar   k _     -> k
        TCon   k _     -> k
        TApp   k _ _   -> k
        TNil           -> kRow
        TExt   {}      -> kRow
        _              -> kTyp
    )

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

class Typed t where
  typeOf :: t -> Type v

{- ORMOLU_DISABLE -}

instance Typed (Type v) where
  typeOf =
    cata
      ( \case
          TUnit        -> tUnit
          TBool        -> tBool
          TInt         -> tInt
          TBig         -> tBig
          TNat         -> tNat
          TFloat       -> tFloat
          TDouble      -> tDouble
          TChar        -> tChar
          TString      -> tString
          TVoid        -> tVoid
          TList t      -> tList t
--          TVar k v     -> tVar k v
          TCon k n     -> tCon k n
          TApp k t1 t2 -> tApp k t1 t2
          TArr t1 t2   -> tArr t1 t2
          TRec r       -> tRec r
          TNil         -> tNil
          TExt n a1 a2 -> tExt n a1 a2
      )

{- ORMOLU_ENABLE -}

instance (Tagged a t, Typed t) => Typed a where
  typeOf = typeOf . getTag

--------------------------------------------------------------------------------

fieldSet :: [(Name, a)] -> FieldSet a
fieldSet = foldr (uncurry (Map.insertWith (<>))) mempty . (singleton <$$>)

constructorEnv :: [(Name, ([Name], Int))] -> ConstructorEnv
constructorEnv = Env.fromList <<< (first Set.fromList <$$>)

unpackRow :: (Row r) => r -> (FieldSet r, r)
unpackRow r = (fieldSet (rInit r), rLast r)

packRow :: (Row r) => (FieldSet r, r) -> r
packRow (m, r) = Map.foldrWithKey (flip . foldr . rExt) r m

normalizeRow :: (Row r) => r -> r
normalizeRow = packRow . unpackRow

tApps :: Type v -> [Type v] -> Type v
tApps = foldl' go
  where
    go t =
      let _ `KArr` k = project (kindOf t)
       in tApp k t

tupleCon :: Int -> Name
tupleCon size = "(" <> Text.replicate (pred size) "," <> ")"

listNil :: (Con a t) => t -> a
listNil t = con t "[]" []

listCons :: (Con a t) => t -> a -> a -> a
listCons t hd tl = con t "(::)" [hd, tl]

rawTuple :: (Con a t) => t -> [a] -> a
rawTuple t ps = con t (tupleCon (length ps)) ps

rawList :: (Con a t) => t -> [a] -> a
rawList t = foldr (listCons t) (listNil t)

rawRow :: (Con a t, Tagged a t, Row a, Row t, Eq a) => a -> a
rawRow a = Map.foldrWithKey (flip . foldr . go) final m
  where
    (m, r) = unpackRow a
    final
      | r == rNil = con rNil "{}" []
      | otherwise = r
    go n p q =
      let t = rExt n (getTag p) (getTag q)
       in con t ("{" <> n <> "}") [p, q]
