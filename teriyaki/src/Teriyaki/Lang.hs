module Teriyaki.Lang where

import Teriyaki.Data
import Teriyaki.Util

--class Typed a where
--  typeOf :: a -> MonoType
--
--instance Typed MonoType where
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
tVar :: TVar v -> Type v
tVar = embed1 TVar

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
pVar :: Label t -> Pattern t
pVar = embed1 PVar

{-# INLINE pLit #-}
pLit :: Prim -> Pattern t
pLit = embed1 PLit

{-# INLINE pAs #-}
pAs :: Pattern t -> Pattern t
pAs = embed1 PAs

{-# INLINE pOr #-}
pOr :: Pattern t -> Pattern t -> Pattern t
pOr = embed2 POr

{-# INLINE pAny #-}
pAny :: Pattern t
pAny = embed PAny

{-# INLINE pCon #-}
pCon :: Label t -> [Pattern t] -> Pattern t
pCon = embed2 PCon

{-# INLINE pTup #-}
pTup :: [Pattern t] -> Pattern t
pTup = embed1 PTup

{-# INLINE pList #-}
pList :: [Pattern t] -> Pattern t
pList = embed1 PList

{-# INLINE pNil #-}
pNil :: Pattern t
pNil = embed PNil

{-# INLINE pExt #-}
pExt :: Name -> Pattern t -> Pattern t -> Pattern t
pExt = embed3 PExt

{-# INLINE pAnn #-}
pAnn :: t -> Pattern t -> Pattern t
pAnn = embed2 PAnn

{-# INLINE eVar #-}
eVar :: Label t -> Expr t
eVar = embed1 EVar

{-# INLINE eCon #-}
eCon :: Label t -> Expr t
eCon = embed1 ECon

{-# INLINE eLit #-}
eLit :: Prim -> Expr t
eLit = embed1 ELit

{-# INLINE eApp #-}
eApp :: t -> Expr t -> [Expr t] -> Expr t
eApp = embed3 EApp

-- TODO
-- {-# INLINE eLam #-}
-- eLam = undefined

{-# INLINE eIf #-}
eIf :: Expr t -> Expr t -> Expr t -> Expr t
eIf = embed3 EIf

-- TODO
-- {-# INLINE ePat #-}
-- ePat = undefined

{-# INLINE eLet #-}
eLet :: Binding t -> Expr t -> Expr t -> Expr t
eLet = embed3 ELet

-- TODO
-- {-# INLINE eFix #-}
-- eFix = undefined
--
-- {-# INLINE eFun #-}
-- eFun = undefined

{-# INLINE eOp1 #-}
eOp1 :: (t, Op1) -> Expr t -> Expr t
eOp1 = embed2 EOp1

{-# INLINE eOp2 #-}
eOp2 :: (t, Op2) -> Expr t -> Expr t -> Expr t
eOp2 = embed3 EOp2

{-# INLINE eTup #-}
eTup :: Expr t
eTup = embed ETup

{-# INLINE eList #-}
eList :: Expr t
eList = embed EList

{-# INLINE eNil #-}
eNil :: Expr t
eNil = embed ENil

{-# INLINE eExt #-}
eExt :: Name -> Expr t -> Expr t -> Expr t
eExt = embed3 EExt

-- {-# INLINE eAnn #-}
-- eAnn :: Type v -> Expr t -> Expr t
-- eAnn = embed2 EAnn

{-# INLINE eSub #-}
eSub :: t -> Expr t
eSub = embed1 ESub

-- TODO
-- {-# INLINE eCo #-}
-- eCo :: Expr t -> Expr t
-- eCo = embed1 ECo
