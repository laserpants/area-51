module Pong.Data.Cons where

import Pong.Data
import Pong.Util
  ( Name
  , embed
  , embed1
  , embed2
  , embed3
  )

{-# INLINE tUnit #-}
tUnit :: Type v
tUnit = embed TUnit

{-# INLINE tBool #-}
tBool :: Type v
tBool = embed TBool

{-# INLINE tInt #-}
tInt :: Type v
tInt = embed TInt

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

{-# INLINE tCon #-}
tCon :: Name -> [Type v] -> Type v
tCon = embed2 TCon

{-# INLINE tArr #-}
tArr :: Type v -> Type v -> Type v
tArr = embed2 TArr

infixr 1 `tArr`

{-# INLINE (~>) #-}
(~>) :: Type v -> Type v -> Type v
(~>) = tArr

infixr 1 ~>

{-# INLINE tVar #-}
tVar :: v -> Type v
tVar = embed1 TVar

{-# INLINE tRec #-}
tRec :: Type v -> Type v
tRec = embed1 TRec

{-# INLINE rNil #-}
rNil :: Type v
rNil = embed RNil

{-# INLINE rExt #-}
rExt :: Name -> Type v -> Type v -> Type v
rExt = embed3 RExt

{-# INLINE eVar #-}
eVar :: Label t -> Expr t a0 a1 a2
eVar = embed1 EVar

{-# INLINE eCon #-}
eCon :: Label a0 -> Expr t a0 a1 a2
eCon = embed1 ECon

{-# INLINE eLit #-}
eLit :: Prim -> Expr t a0 a1 a2
eLit = embed1 ELit

{-# INLINE eIf #-}
eIf :: Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eIf = embed3 EIf

{-# INLINE eLet #-}
eLet :: Label t -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eLet = embed3 ELet

{-# INLINE eLam #-}
eLam :: a1 -> [Label t] -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eLam = embed3 ELam

{-# INLINE eApp #-}
eApp :: t1 -> Expr t0 t1 a1 a2 -> [Expr t0 t1 a1 a2] -> Expr t0 t1 a1 a2
eApp = embed3 EApp

{-# INLINE eCall #-}
eCall :: a2 -> Label t -> [Expr t a0 a1 a2] -> Expr t a0 a1 a2
eCall = embed3 ECall

{-# INLINE eOp1 #-}
eOp1 :: (t, Op1) -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eOp1 = embed2 EOp1

{-# INLINE eOp2 #-}
eOp2 :: (t, Op2) -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eOp2 = embed3 EOp2

{-# INLINE ePat #-}
ePat :: Expr t a0 a1 a2 -> [Clause t (Expr t a0 a1 a2)] -> Expr t a0 a1 a2
ePat = embed2 EPat

{-# INLINE eNil #-}
eNil :: Expr t a0 a1 a2
eNil = embed ENil

{-# INLINE eExt #-}
eExt :: Name -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eExt = embed3 EExt

{-# INLINE eRes #-}
eRes :: [Label t] -> Expr t a0 a1 a2 -> Expr t a0 a1 a2 -> Expr t a0 a1 a2
eRes = embed3 ERes

{-# INLINE oAddInt #-}
oAddInt :: (Type v, Op2)
oAddInt = (tInt ~> tInt ~> tInt, OAdd)

{-# INLINE oSubInt #-}
oSubInt :: (Type v, Op2)
oSubInt = (tInt ~> tInt ~> tInt, OSub)

{-# INLINE oMulInt #-}
oMulInt :: (Type v, Op2)
oMulInt = (tInt ~> tInt ~> tInt, OMul)

{-# INLINE oEqInt #-}
oEqInt :: (Type v, Op2)
oEqInt = (tInt ~> tInt ~> tBool, OEq)

{-# INLINE oNEqInt #-}
oNEqInt :: (Type v, Op2)
oNEqInt = (tInt ~> tInt ~> tBool, ONEq)

{-# INLINE oLtInt #-}
oLtInt :: (Type v, Op2)
oLtInt = (tInt ~> tInt ~> tBool, OLt)

{-# INLINE oGtInt #-}
oGtInt :: (Type v, Op2)
oGtInt = (tInt ~> tInt ~> tBool, OGt)

{-# INLINE oLtEInt #-}
oLtEInt :: (Type v, Op2)
oLtEInt = (tInt ~> tInt ~> tBool, OLtE)

{-# INLINE oGtEInt #-}
oGtEInt :: (Type v, Op2)
oGtEInt = (tInt ~> tInt ~> tBool, OGtE)
