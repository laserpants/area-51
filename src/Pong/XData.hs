{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Pong.XData where

import Data.Eq.Deriving (deriveEq1)
import Data.List.NonEmpty
import Data.Ord.Deriving (deriveOrd1)
import Data.Void (Void)
import Pong.Util (Fix(..), Name, Text, embed, embed1, embed2, embed3, embed4)
import Text.Show.Deriving (deriveShow1)

data XRowF r a
  = XRNil
  | XRVar Name
  | XRExt Name r a

type XRow r = Fix (XRowF r)

data XTypeF a
  = XTUnit
  | XTBool
  | XTInt32
  | XTInt64
  | XTFloat
  | XTDouble
  | XTChar
  | XTString
  | XTArr a a
  | XTVar Int
  | XTGen Int
  | XTData Name
  | XTRow (XRow XType)

type XType = Fix XTypeF 

data XLiteral
  = XLBool Bool
  | XLInt32 Int
  | XLInt64 Int
  | XLFloat Float
  | XLDouble Double
  | XLChar Char
  | XLString Text
  | XLUnit

data XOp2
  = XOEqInt32
  | XOAddInt32
  | XOSubInt32
  | XOMulInt32
  | XOAddFloat
  | XOMulFloat
  | XOSubFloat
  | XODivFloat
  | XOAddDouble
  | XOMulDouble
  | XOSubDouble
  | XODivDouble

type XLabel t = (t, Name)

data XExprF t a0 a1 a2 a
  = XEVar (XLabel t)
  | XELit XLiteral
  | XEIf a a a
  | XELet (XLabel t) a a
  | XELam a0 [XLabel t] a
  | XEApp a1 a [a]
  | XECall a2 (XLabel t) [a]
  | XEOp2 XOp2 a a
  | XECase a [([XLabel t], a)]
  | XERow (XRow (XExpr t a0 a1 a2))

type XExpr t a0 a1 a2 = Fix (XExprF t a0 a1 a2)

--data XSignature r a =
--  XSignature
--    { arguments :: NonEmpty r
--    , body :: a
--    }

--data XConstructor =
--  XConstructor
--    { consName :: Name
--    , consFields :: [XPolyType]
--    }

data XDefinition r a
  = XFunction (NonEmpty r) a
--  = XFunction -- (XSignature (XLabel XPolyType) (XPolyType, a))
--  | XExternal 
--  | XConstant XLiteral 
--  | XData Name [XConstructor]

-- XRow
deriving instance (Show r, Show a) => Show (XRowF r a)

deriving instance (Eq r, Eq a) => Eq (XRowF r a)

deriving instance (Ord r, Ord a) => Ord (XRowF r a)

deriveShow1 ''XRowF

deriveEq1 ''XRowF

deriveOrd1 ''XRowF

deriving instance Functor (XRowF r)

deriving instance Foldable (XRowF r)

deriving instance Traversable (XRowF r)

-- XType
deriving instance (Show a) => Show (XTypeF a)

deriving instance (Eq a) => Eq (XTypeF a)

deriving instance (Ord a) => Ord (XTypeF a)

deriveShow1 ''XTypeF

deriveEq1 ''XTypeF

deriveOrd1 ''XTypeF

deriving instance Functor XTypeF

deriving instance Foldable XTypeF

deriving instance Traversable XTypeF

-- XLiteral
deriving instance Show XLiteral

deriving instance Eq XLiteral

deriving instance Ord XLiteral

-- Op2
deriving instance Show XOp2

deriving instance Eq XOp2

deriving instance Ord XOp2

-- XExpr
deriving instance (Show t, Show a0, Show a1, Show a2, Show a) => Show (XExprF t a0 a1 a2 a)

deriving instance (Eq t, Eq a0, Eq a1, Eq a2, Eq a) => Eq (XExprF t a0 a1 a2 a)

deriving instance (Ord t, Ord a0, Ord a1, Ord a2, Ord a) => Ord (XExprF t a0 a1 a2 a)

deriveShow1 ''XExprF

deriveEq1 ''XExprF

deriveOrd1 ''XExprF

deriving instance Functor (XExprF t a0 a1 a2)

deriving instance Foldable (XExprF t a0 a1 a2)

deriving instance Traversable (XExprF t a0 a1 a2)

-- XDefinition
deriving instance (Show r, Show a) => Show (XDefinition r a)

deriving instance (Eq r, Eq a) => Eq (XDefinition r a)

deriving instance Functor (XDefinition r)

deriving instance Foldable (XDefinition r)

deriving instance Traversable (XDefinition r)

--

xtInt32 :: XType
xtInt32 = embed XTInt32

xtArr :: XType -> XType -> XType
xtArr = embed2 XTArr

infixr 1 `xtArr`

(~~>) = xtArr
-- TODO ~>

xtVar :: Int -> XType
xtVar = embed1 XTVar

xtGen :: Int -> XType
xtGen = embed1 XTGen

xeOp2 :: XOp2 -> XExpr t a0 a1 a2 -> XExpr t a0 a1 a2 -> XExpr t a0 a1 a2
xeOp2 = embed3 XEOp2

xeVar :: XLabel t -> XExpr t a0 a1 a2
xeVar = embed1 XEVar

xeLit :: XLiteral -> XExpr t a0 a1 a2 
xeLit = embed1 XELit 

xeLet :: XLabel t -> XExpr t a0 a1 a2 -> XExpr t a0 a1 a2 -> XExpr t a0 a1 a2 
xeLet = embed3 XELet 

xeLam :: [XLabel t] -> XExpr t () a1 a2 -> XExpr t () a1 a2
xeLam = embed3 XELam ()

xeApp :: XExpr t a0 () a2 -> [XExpr t a0 () a2] -> XExpr t a0 () a2
xeApp = embed3 XEApp ()

