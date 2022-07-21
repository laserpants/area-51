{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Teriyaki.Lang where

import Teriyaki.Data
import Teriyaki.Util

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
          PAs   t _          -> t
          POr   t _ _        -> t
          PAny  t            -> t
          PCon  t _ _        -> t
          PTup  t _          -> t
          PList t _          -> t
          PNil  t            -> t
          PExt  t _ _ _      -> t
          PAnn  t _          -> t
      )

  setTag t =
    cata
      ( \case
          PVar  _ a1         -> pVar  t a1
          PLit  _ a1         -> pLit  t a1
          PAs   _ a1         -> pAs   t a1
          POr   _ a1 a2      -> pOr   t a1 a2
          PAny  _            -> pAny  t
          PCon  _ a1 a2      -> pCon  t a1 a2
          PTup  _ a1         -> pTup  t a1
          PList _ a1         -> pList t a1
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
          EPat  t            -> t
          ELet  t _ _ _      -> t
          EFix  t _ _ _      -> t
          EFun  t            -> t
          EOp1  t _ _        -> t
          EOp2  t _ _ _      -> t
          ETup  t            -> t
          EList t            -> t
          ENil  t            -> t
          EExt  t _ _ _      -> t
          ESub  t            -> t
          ECo   t _          -> t
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
          EPat  _            -> undefined -- TODO
          ELet  _ a1 a2 a3   -> eLet  t a1 a2 a3
          EFix  _ a1 a2 a3   -> eFix  t a1 a2 a3
          EFun  _            -> undefined -- TODO
          EOp1  _ a1 a2      -> eOp1  t a1 a2
          EOp2  _ a1 a2 a3   -> eOp2  t a1 a2 a3
          ETup  _            -> eTup  t
          EList _            -> eList t
          ENil  _            -> eNil  t
          EExt  _ a1 a2 a3   -> eExt  t a1 a2 a3
          ESub  _            -> eSub  t
          ECo   _ a1         -> eCo   t a1
      )


{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

-- class Typed a where
--  typeOf :: a -> MonoType
--
-- instance Typed MonoType where
--  typeOf = id

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
tTup :: Type v
tTup = embed TTup

{-# INLINE tList #-}
tList :: Type v
tList = embed TList

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
pAs :: t -> Pattern t -> Pattern t
pAs = embed2 PAs

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

-- TODO
{-# INLINE ePat #-}
ePat :: t -> Expr t
ePat = embed1 EPat

{-# INLINE eLet #-}
eLet :: t -> Binding t -> Expr t -> Expr t -> Expr t
eLet = embed4 ELet

{-# INLINE eFix #-}
eFix :: t -> Name -> Expr t -> Expr t -> Expr t
eFix = embed4 EFix

-- TODO
{-# INLINE eFun #-}
eFun :: t -> Expr t
eFun = embed1 EFun

{-# INLINE eOp1 #-}
eOp1 :: t -> Op1 t -> Expr t -> Expr t
eOp1 = embed3 EOp1

{-# INLINE eOp2 #-}
eOp2 :: t -> Op2 t -> Expr t -> Expr t -> Expr t
eOp2 = embed4 EOp2

{-# INLINE eTup #-}
eTup :: t -> Expr t
eTup = embed1 ETup

{-# INLINE eList #-}
eList :: t -> Expr t
eList = embed1 EList

{-# INLINE eNil #-}
eNil :: t -> Expr t
eNil = embed1 ENil

{-# INLINE eExt #-}
eExt :: t -> Name -> Expr t -> Expr t -> Expr t
eExt = embed4 EExt

---- {-# INLINE eAnn #-}
---- eAnn :: Type v -> Expr t -> Expr t
---- eAnn = embed2 EAnn

{-# INLINE eSub #-}
eSub :: t -> Expr t
eSub = embed1 ESub

{-# INLINE eCo #-}
eCo :: t -> Expr t -> Expr t
eCo = embed2 ECo
