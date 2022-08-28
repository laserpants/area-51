module Taiyaki.Data.Cons where

import Taiyaki.Data
import Taiyaki.Util
  ( Name
  , embed
  , embed1
  , embed2
  , embed3
  , embed4
  )

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

{-# INLINE kFun #-}
kFun :: Int -> Kind
kFun n = foldr1 kArr (replicate (succ n) kTyp)

{-# INLINE kFun1 #-}
kFun1 :: Kind
kFun1 = kFun 1

{-# INLINE kFun2 #-}
kFun2 :: Kind
kFun2 = kFun 2

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

infixr 1 ~>

{-# INLINE tRec #-}
tRec :: Type v -> Type v
tRec = embed1 TRec

{-# INLINE tNil #-}
tNil :: Type v
tNil = embed TNil

{-# INLINE tExt #-}
tExt :: Name -> Type v -> Type v -> Type v
tExt = embed3 TExt

{-# INLINE tListApp #-}
tListApp :: Type v -> Type v
tListApp = tApp kTyp tList

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
eVar :: (Functor e2, Functor e3) => t -> Name -> Expr t e1 e2 e3 e4
eVar = embed2 EVar

{-# INLINE eCon #-}
eCon :: (Functor e2, Functor e3) => t -> Name -> Expr t e1 e2 e3 e4
eCon = embed2 ECon

{-# INLINE eLit #-}
eLit :: (Functor e2, Functor e3) => t -> Prim -> Expr t e1 e2 e3 e4
eLit = embed2 ELit

{-# INLINE eApp #-}
eApp ::
  (Functor e2, Functor e3) =>
  t ->
  Expr t e1 e2 e3 e4 ->
  [Expr t e1 e2 e3 e4] ->
  Expr t e1 e2 e3 e4
eApp = embed3 EApp

{-# INLINE eLam #-}
eLam ::
  (Functor e2, Functor e3) =>
  t ->
  e1 ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eLam = embed3 ELam

{-# INLINE eIf #-}
eIf ::
  (Functor e2, Functor e3) =>
  t ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eIf = embed4 EIf

{-# INLINE ePat #-}
ePat ::
  (Functor e2, Functor e3) =>
  t ->
  Expr t e1 e2 e3 e4 ->
  [e2 (Expr t e1 e2 e3 e4)] ->
  Expr t e1 e2 e3 e4
ePat = embed3 EPat

{-# INLINE eLet #-}
eLet ::
  (Functor e2, Functor e3) =>
  t ->
  e4 ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eLet = embed4 ELet

{-# INLINE eFix #-}
eFix ::
  (Functor e2, Functor e3) =>
  t ->
  Name ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eFix = embed4 EFix

{-# INLINE eFun #-}
eFun ::
  (Functor e2, Functor e3) =>
  t ->
  [e3 (Expr t e1 e2 e3 e4)] ->
  Expr t e1 e2 e3 e4
eFun = embed2 EFun

{-# INLINE eOp1 #-}
eOp1 ::
  (Functor e2, Functor e3) =>
  t ->
  Op1 t ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eOp1 = embed3 EOp1

{-# INLINE eOp2 #-}
eOp2 ::
  (Functor e2, Functor e3) =>
  t ->
  Op2 t ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eOp2 = embed4 EOp2

{-# INLINE eTup #-}
eTup ::
  (Functor e2, Functor e3) =>
  t ->
  [Expr t e1 e2 e3 e4] ->
  Expr t e1 e2 e3 e4
eTup = embed2 ETup

{-# INLINE eList #-}
eList ::
  (Functor e2, Functor e3) =>
  t ->
  [Expr t e1 e2 e3 e4] ->
  Expr t e1 e2 e3 e4
eList = embed2 EList

{-# INLINE eRec #-}
eRec ::
  (Functor e2, Functor e3) =>
  t ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eRec = embed2 ERec

{-# INLINE eNil #-}
eNil :: (Functor e2, Functor e3) => t -> Expr t e1 e2 e3 e4
eNil = embed1 ENil

{-# INLINE eExt #-}
eExt ::
  (Functor e2, Functor e3) =>
  t ->
  Name ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eExt = embed4 EExt

{-# INLINE eAnn #-}
eAnn ::
  (Functor e2, Functor e3) =>
  t ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eAnn = embed2 EAnn

{-# INLINE eSub #-}
eSub :: (Functor e2, Functor e3) => t -> Expr t e1 e2 e3 e4
eSub = embed1 ESub

{-# INLINE eCo #-}
eCo ::
  (Functor e2, Functor e3) =>
  t ->
  Expr t e1 e2 e3 e4 ->
  Expr t e1 e2 e3 e4
eCo = embed2 ECo
