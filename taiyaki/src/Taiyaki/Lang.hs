{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
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

class Tagged a t | a -> t where
  getTag :: a -> t
  setTag :: t -> a -> a

{- ORMOLU_DISABLE -}

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

  setTag t =
    cata
      ( \case
          PVar  _ a1         -> pVar  t a1
          PLit  _ a1         -> pLit  t a1
          PAs   _ a1 a2      -> pAs   t a1 a2
          POr   _ a1 a2      -> pOr   t a1 a2
          PAny  _            -> pAny  t
          PCon  _ a1 a2      -> pCon  t a1 a2
          PTup  _ a1         -> pTup  t a1
          PList _ a1         -> pList t a1
          PRec  _ a1         -> pRec  t a1
          PNil  _            -> pNil  t
          PExt  _ a1 a2 a3   -> pExt  t a1 a2 a3
          PAnn  _ a1         -> pAnn  t a1
      )

instance Tagged (Binding t) t where
  getTag =
    \case
      BPat t _               -> t
      BFun t _ _             -> t

  setTag t =
    \case
      BPat _ a1              -> BPat t a1
      BFun _ a1 a2           -> BFun t a1 a2

instance Tagged (Clause t) t where
  getTag =
    \case
      Clause t _ _           -> t

  setTag t =
    \case
      Clause _ a1 a2         -> Clause t a1 a2

instance Tagged (Op1 t) t where
  getTag =
    \case
      ONot t                 -> t
      ONeg t                 -> t

  setTag t =
    \case
      ONot _                 -> ONot t
      ONeg _                 -> ONeg t

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

  setTag t =
    \case
      OEq   _                -> OEq   t
      ONEq  _                -> ONEq  t
      OLt   _                -> OLt   t
      OGt   _                -> OGt   t
      OLtE  _                -> OLtE  t
      OGtE  _                -> OGtE  t
      OAdd  _                -> OAdd  t
      OSub  _                -> OSub  t
      OMul  _                -> OMul  t
      ODiv  _                -> ODiv  t
      OPow  _                -> OPow  t
      OMod  _                -> OMod  t
      OOr   _                -> OOr   t
      OAnd  _                -> OAnd  t
      OLArr _                -> OLArr t
      ORarr _                -> ORarr t
      OFPip _                -> OFPip t
      OBPip _                -> OBPip t
      ODot  _                -> ODot  t
      OGet  _                -> OGet  t

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

  setTag t =
    cata
      ( \case
          EVar  _ a1         -> eVar  t a1
          ECon  _ a1         -> eCon  t a1
          ELit  _ a1         -> eLit  t a1
          EApp  _ a1 a2      -> eApp  t a1 a2
          ELam  _ a1 a2      -> eLam  t a1 a2
          EIf   _ a1 a2 a3   -> eIf   t a1 a2 a3
          EPat  _ a1 a2      -> ePat  t a1 a2
          ELet  _ a1 a2 a3   -> eLet  t a1 a2 a3
          EFix  _ a1 a2 a3   -> eFix  t a1 a2 a3
          EFun  _ a1         -> eFun  t a1
          EOp1  _ a1 a2      -> eOp1  t a1 a2
          EOp2  _ a1 a2 a3   -> eOp2  t a1 a2 a3
          ETup  _ a1         -> eTup  t a1
          EList _ a1         -> eList t a1
          ERec  _ a1         -> eRec  t a1
          ENil  _            -> eNil  t
          EExt  _ a1 a2 a3   -> eExt  t a1 a2 a3
          ESub  _            -> eSub  t
          ECo   _ a1         -> eCo   t a1
          EAnn  _ a1         -> eAnn  t a1
      )

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

class Typed t v where
  typeOf :: t -> Type v

{- ORMOLU_DISABLE -}

instance Typed (Type v) v where
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
          TVar k v     -> tVar k v
          TCon k n     -> tCon k n
          TApp k t1 t2 -> tApp k t1 t2
          TArr t1 t2   -> tArr t1 t2
          TRec r       -> tRec r
          TNil         -> tNil
          TExt n a1 a2 -> tExt n a1 a2
      )

{- ORMOLU_ENABLE -}

instance (Tagged a t, Typed t v) => Typed a v where
  typeOf = typeOf . getTag

--------------------------------------------------------------------------------

fieldSet :: [(Name, a)] -> FieldSet a
fieldSet = foldr (uncurry (Map.insertWith (<>))) mempty . (singleton <$$>)

constructorEnv :: [(Name, ([Name], Int))] -> ConstructorEnv
constructorEnv = Env.fromList <<< (first Set.fromList <$$>)

unpackRow :: (Row r) => r -> (FieldSet r, r)
unpackRow r = (fieldSet (rInit r), rLast r)

tApps :: Type v -> [Type v] -> Type v
tApps = foldl' go
  where
    go t =
      let _ `KArr` k = project (kindOf t)
       in tApp k t

tupleCon :: Int -> Name
tupleCon size = "(" <> Text.replicate (pred size) "," <> ")"

foldTuple :: (Con a t) => t -> [a] -> a
foldTuple t ps = con t (tupleCon (length ps)) ps

listNil :: (Con a t) => t -> a
listNil t = con t "[]" []

listCons :: (Con a t) => t -> a -> a -> a
listCons t head_ tail_ = con t "(::)" [head_, tail_]

foldList :: (Con a t) => t -> [a] -> a
foldList t = foldr (listCons t) (listNil t)

-- foldRow :: (Con a t, Tagged a t, Row a, Row t, Eq a) => a -> a
-- foldRow a = Map.foldrWithKey (flip . foldr . go) last_ m
--  where
--    (m, r) = unpackRow a
--    last_
--      | r == rNil = con rNil "{}" []
--      | otherwise = r
--    go n p q =
--      let t = rExt n (getTag p) (getTag q)
--       in con t ("{" <> n <> "}") [p, q]
--
-- foldRecord :: (Row t, Eq t) => Pattern t -> Pattern t
-- foldRecord =
--  project
--    >>> \case
--      PRec t p -> pCon t "#{*}" [foldRow p]
--      _ -> error "Not a record"
