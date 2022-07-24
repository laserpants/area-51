{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Teriyaki.Lang where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Teriyaki.Data
import Teriyaki.Util

-------------------------------------------------------------------------------

fieldSet :: [(Name, a)] -> FieldSet a
fieldSet = foldr (uncurry (Map.insertWith (<>))) mempty . (singleton <$$>)

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

unwindRow :: (Row r) => r -> (FieldSet r, r)
unwindRow r = (fieldSet (rInit r), rLast r)

-------------------------------------------------------------------------------

class Tuple t a | t -> a where
  tup :: a -> [t] -> t

instance Tuple () () where
  tup _ _ = ()

instance Tuple (Type v) () where
  tup = const tTup

instance Tuple (Expr t) t where
  tup = eTup

instance Tuple (Pattern t) t where
  tup = pTup

-------------------------------------------------------------------------------

class Tagged t a | t -> a where
  getTag :: t -> a
  setTag :: a -> t -> t

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

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

-- class Typed a where
--  typeOf :: a -> MonoType
--
-- instance Typed MonoType where
--  typeOf = id

-------------------------------------------------------------------------------

pListNil :: t -> Pattern t
pListNil t = pCon t "[]" []

pListCons :: t -> Pattern t -> Pattern t -> Pattern t
pListCons t head_ tail_ = pCon t "(::)" [head_, tail_]

tupleCon :: Int -> Name
tupleCon size = "(" <> Text.replicate (pred size) "," <> ")"

foldTuple :: t -> [Pattern t] -> Pattern t
foldTuple t ps = pCon t (tupleCon (length ps)) ps

foldList :: t -> [Pattern t] -> Pattern t
foldList t = foldr (pListCons t) (pListNil t)

foldRow :: (Row t) => Pattern t -> Pattern t
foldRow a = Map.foldrWithKey (flip . foldr . go) leaf m
  where
    (m, r) = unwindRow a
    leaf =
      case project r of
        PNil t -> pCon t "{}" []
        _ -> r
    go n p q =
      let t = rExt n (getTag p) (getTag q)
       in pCon t ("{" <> n <> "}") [p, q]

foldRecord :: (Row t) => Pattern t -> Pattern t
foldRecord =
  project
    >>> \case
      PRec t p -> pCon t "#{*}" [foldRow p]
      _ -> error "Implementation error"

-------------------------------------------------------------------------------

{-# INLINE kTyp #-}
kTyp :: Kind
kTyp = embed KTyp

{-# INLINE kRow #-}
kRow :: Kind
kRow = embed KRow

{-# INLINE kArr #-}
kArr :: Kind -> Kind -> Kind
kArr = embed2 KArr

infixr 1 `kArr`

{-# INLINE tUnit #-}
tUnit :: Type v
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: Type v
tBool = embed TBool

{-# INLINE tInt #-}
tInt :: Type v
tInt = embed TInt

{-# INLINE tBig #-}
tBig :: Type v
tBig = embed TBig

{-# INLINE tNat #-}
tNat :: Type v
tNat = embed TNat

{-# INLINE tFloat #-}
tFloat :: Type v
tFloat = embed TFloat

{-# INLINE tDouble #-}
tDouble :: Type v
tDouble = embed TDouble

{-# INLINE tChar #-}
tChar :: Type v
tChar = embed TChar

{-# INLINE tString #-}
tString :: Type v
tString = embed TString

{-# INLINE tVoid #-}
tVoid :: Type v
tVoid = embed TVoid

{-# INLINE tTup #-}
tTup :: [Type v] -> Type v
tTup = embed1 TTup

{-# INLINE tList #-}
tList :: Type v -> Type v
tList = embed1 TList

{-# INLINE tVar #-}
tVar :: Kind -> v -> Type v
tVar = embed2 TVar

{-# INLINE tCon #-}
tCon :: Kind -> Name -> Type v
tCon = embed2 TCon

{-# INLINE tApp #-}
tApp :: Kind -> Type v -> Type v -> Type v
tApp = embed3 TApp

{-# INLINE tArr #-}
tArr :: Type v -> Type v -> Type v
tArr = embed2 TArr

infixr 1 `tArr`

{-# INLINE (~>) #-}
(~>) :: Type v -> Type v -> Type v
(~>) = tArr

{-# INLINE tRec #-}
tRec :: Type v -> Type v
tRec = embed1 TRec

{-# INLINE tNil #-}
tNil :: Type v
tNil = embed TNil

{-# INLINE tExt #-}
tExt :: Name -> Type v -> Type v -> Type v
tExt = embed3 TExt

{-# INLINE pVar #-}
pVar :: t -> Name -> Pattern t
pVar = embed2 PVar

{-# INLINE pLit #-}
pLit :: t -> Prim -> Pattern t
pLit = embed2 PLit

{-# INLINE pAs #-}
pAs :: t -> Name -> Pattern t -> Pattern t
pAs = embed3 PAs

{-# INLINE pOr #-}
pOr :: t -> Pattern t -> Pattern t -> Pattern t
pOr = embed3 POr

{-# INLINE pAny #-}
pAny :: t -> Pattern t
pAny = embed1 PAny

{-# INLINE pCon #-}
pCon :: t -> Name -> [Pattern t] -> Pattern t
pCon = embed3 PCon

{-# INLINE pTup #-}
pTup :: t -> [Pattern t] -> Pattern t
pTup = embed2 PTup

{-# INLINE pList #-}
pList :: t -> [Pattern t] -> Pattern t
pList = embed2 PList

{-# INLINE pRec #-}
pRec :: t -> Pattern t -> Pattern t
pRec = embed2 PRec

{-# INLINE pNil #-}
pNil :: t -> Pattern t
pNil = embed1 PNil

{-# INLINE pExt #-}
pExt :: t -> Name -> Pattern t -> Pattern t -> Pattern t
pExt = embed4 PExt

{-# INLINE pAnn #-}
pAnn :: t -> Pattern t -> Pattern t
pAnn = embed2 PAnn

{-# INLINE eVar #-}
eVar :: t -> Name -> Expr t
eVar = embed2 EVar

{-# INLINE eCon #-}
eCon :: t -> Name -> Expr t
eCon = embed2 ECon

{-# INLINE eLit #-}
eLit :: t -> Prim -> Expr t
eLit = embed2 ELit

{-# INLINE eApp #-}
eApp :: t -> Expr t -> [Expr t] -> Expr t
eApp = embed3 EApp

{-# INLINE eLam #-}
eLam :: t -> [Pattern t] -> Expr t -> Expr t
eLam = embed3 ELam

{-# INLINE eIf #-}
eIf :: t -> Expr t -> Expr t -> Expr t -> Expr t
eIf = embed4 EIf

{-# INLINE ePat #-}
ePat :: t -> Expr t -> [Clause t] -> Expr t
ePat = embed3 EPat

{-# INLINE eLet #-}
eLet :: t -> Binding t -> Expr t -> Expr t -> Expr t
eLet = embed4 ELet

{-# INLINE eFix #-}
eFix :: t -> Name -> Expr t -> Expr t -> Expr t
eFix = embed4 EFix

{-# INLINE eFun #-}
eFun :: t -> [Clause t] -> Expr t
eFun = embed2 EFun

{-# INLINE eOp1 #-}
eOp1 :: t -> Op1 t -> Expr t -> Expr t
eOp1 = embed3 EOp1

{-# INLINE eOp2 #-}
eOp2 :: t -> Op2 t -> Expr t -> Expr t -> Expr t
eOp2 = embed4 EOp2

{-# INLINE eTup #-}
eTup :: t -> [Expr t] -> Expr t
eTup = embed2 ETup

{-# INLINE eList #-}
eList :: t -> [Expr t] -> Expr t
eList = embed2 EList

{-# INLINE eRec #-}
eRec :: t -> Expr t -> Expr t
eRec = embed2 ERec

{-# INLINE eNil #-}
eNil :: t -> Expr t
eNil = embed1 ENil

{-# INLINE eExt #-}
eExt :: t -> Name -> Expr t -> Expr t -> Expr t
eExt = embed4 EExt

{-# INLINE eAnn #-}
eAnn :: t -> Expr t -> Expr t
eAnn = embed2 EAnn

{-# INLINE eSub #-}
eSub :: t -> Expr t
eSub = embed1 ESub

{-# INLINE eCo #-}
eCo :: t -> Expr t -> Expr t
eCo = embed2 ECo
