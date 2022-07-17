module Teriyaki.Lang where

import Teriyaki.Data
import Teriyaki.Util

{-# INLINE kTyp #-}
kTyp :: Kind
kTyp = embed KTyp

{-# INLINE kRow #-}
kRow :: Kind
kRow = embed KRow

{-# INLINE kArr #-}
kArr :: Kind -> Kind -> Kind
kArr = embed2 KArr

{-# INLINE tUnit #-}
tUnit :: Type
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: Type
tBool = embed TBool

{-# INLINE tInt #-}
tInt :: Type
tInt = embed TInt

{-# INLINE tBig #-}
tBig :: Type
tBig = embed TBig

{-# INLINE tNat #-}
tNat :: Type
tNat = embed TNat

{-# INLINE tFloat #-}
tFloat :: Type
tFloat = embed TFloat

{-# INLINE tDouble #-}
tDouble :: Type
tDouble = embed TDouble

{-# INLINE tChar #-}
tChar :: Type
tChar = embed TChar

{-# INLINE tString #-}
tString :: Type
tString = embed TString

{-# INLINE tVoid #-}
tVoid :: Type
tVoid = embed TVoid

{-# INLINE tTup #-}
tTup :: Type
tTup = embed TTup

{-# INLINE tList #-}
tList :: Type
tList = embed TList

{-# INLINE tVar #-}
tVar :: Kind -> Name -> Type
tVar = embed2 TVar

{-# INLINE tCon #-}
tCon :: Kind -> Name -> Type
tCon = embed2 TCon

{-# INLINE tApp #-}
tApp :: Kind -> Type -> Type -> Type
tApp = embed3 TApp

{-# INLINE tArr #-}
tArr :: Type -> Type -> Type
tArr = embed2 TArr

{-# INLINE tRec #-}
tRec :: Type -> Type
tRec = embed1 TRec

{-# INLINE tNil #-}
tNil :: Type
tNil = embed TNil

{-# INLINE tExt #-}
tExt :: Name -> Type -> Type -> Type
tExt = embed3 TExt

{-# INLINE pVar #-}
pVar :: Label -> Pattern
pVar = embed1 PVar

{-# INLINE pLit #-}
pLit :: Prim -> Pattern
pLit = embed1 PLit

{-# INLINE pAs #-}
pAs :: Pattern -> Pattern
pAs = embed1 PAs

{-# INLINE pOr #-}
pOr :: Pattern -> Pattern -> Pattern
pOr = embed2 POr

{-# INLINE pAny #-}
pAny :: Pattern
pAny = embed PAny

{-# INLINE pCon #-}
pCon :: Label -> [Pattern] -> Pattern
pCon = embed2 PCon

{-# INLINE pTup #-}
pTup :: [Pattern] -> Pattern
pTup = embed1 PTup

{-# INLINE pList #-}
pList :: [Pattern] -> Pattern
pList = embed1 PList

{-# INLINE pNil #-}
pNil :: Pattern
pNil = embed PNil

{-# INLINE pExt #-}
pExt :: Name -> Pattern -> Pattern -> Pattern
pExt = embed3 PExt

{-# INLINE pAnn #-}
pAnn :: Type -> Pattern -> Pattern
pAnn = embed2 PAnn

{-# INLINE eVar #-}
eVar :: Label -> Expr
eVar = embed1 EVar

{-# INLINE eCon #-}
eCon :: Label -> Expr
eCon = embed1 ECon

{-# INLINE eLit #-}
eLit :: Prim -> Expr
eLit = embed1 ELit

{-# INLINE eApp #-}
eApp :: Type -> Expr -> [Expr] -> Expr
eApp = embed3 EApp

-- TODO
-- {-# INLINE eLam #-}
-- eLam = undefined

{-# INLINE eIf #-}
eIf :: Expr -> Expr -> Expr -> Expr
eIf = embed3 EIf

-- TODO
-- {-# INLINE ePat #-}
-- ePat = undefined

{-# INLINE eLet #-}
eLet :: Binding -> Expr -> Expr -> Expr
eLet = embed3 ELet

-- TODO
-- {-# INLINE eFix #-}
-- eFix = undefined
--
-- {-# INLINE eFun #-}
-- eFun = undefined

{-# INLINE eOp1 #-}
eOp1 :: (Type, Op1) -> Expr -> Expr
eOp1 = embed2 EOp1

{-# INLINE eOp2 #-}
eOp2 :: (Type, Op2) -> Expr -> Expr -> Expr
eOp2 = embed3 EOp2

{-# INLINE eTup #-}
eTup :: Expr
eTup = embed ETup

{-# INLINE eList #-}
eList :: Expr
eList = embed EList

{-# INLINE eNil #-}
eNil :: Expr
eNil = embed ENil

{-# INLINE eExt #-}
eExt :: Name -> Expr -> Expr -> Expr
eExt = embed3 EExt

{-# INLINE eAnn #-}
eAnn :: Type -> Expr -> Expr
eAnn = embed2 EAnn

{-# INLINE eSub #-}
eSub :: Type -> Expr
eSub = embed1 ESub

-- TODO
-- {-# INLINE eCo #-}
-- eCo :: Expr -> Expr
-- eCo = embed1 ECo
