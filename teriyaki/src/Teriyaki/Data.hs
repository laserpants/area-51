{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Teriyaki.Data where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Teriyaki.Util

-------------------------------------------------------------------------------

data KindF a
  = KTyp
  | KRow
  | KArr a a

type Kind = Fix KindF

-------------------------------------------------------------------------------

data TypeF a
  = TUnit
  | TBool
  | TInt
  | TBig
  | TNat
  | TFloat
  | TDouble
  | TChar
  | TString
  | TVoid
  | TTup
  | TList
  | TVar Kind Name
  | TCon Kind Name
  | TApp Kind a a
  | TArr a a
  | TRec a
  | TNil
  | TExt Name a a

type Type = Fix TypeF

-------------------------------------------------------------------------------

data Prim
  = RUnit
  | RBool Bool
  | RInt Int
  | RBig Integer
  | RNat Integer
  | RFloat Float
  | RDouble Double
  | RChar Char
  | RString Text

-------------------------------------------------------------------------------

type Label = (Type, Name)

-------------------------------------------------------------------------------

data PatternF a
  = PVar Label
  | PLit Prim
  | PAs a
  | POr a a
  | PAny
  | PCon Label [a]
  | PTup [a]
  | PList [a]
  | PNil
  | PExt Name a a
  | PAnn Type a

type Pattern = Fix PatternF

-------------------------------------------------------------------------------

data ExprF a
  = EVar Label
  | ECon Label
  | ELit Prim
  | EApp Type a [a]
  | ELam Type [Pattern] a
  | EIf a a a
  | EPat -- ?
  | ELet Label a a
  | EFix Label a a
  | EFun -- ?
  | EOp1 (Type, Op1) a
  | EOp2 (Type, Op2) a a
  | ETup
  | EList
  | ENil
  | EExt Name a a
  | EAnn Type a

-- TODO:
-- Codata?
-- Hole?

type Expr = Fix ExprF

-------------------------------------------------------------------------------

data Op1
  = ONot
  | ONeg

-------------------------------------------------------------------------------

data Op2
  = OEq
  | ONEq
  | OLt
  | OGt
  | OLtE
  | OGtE
  | OAdd
  | OSub
  | OMul
  | ODiv
  | OPow
  | OMod
  | OOr
  | OAnd
  | OLArr
  | ORarr
  | OFPip
  | OBPip
  | ODot
  | OGet

-------------------------------------------------------------------------------

data Assoc
  = AL
  | AR
  | ANone

-------------------------------------------------------------------------------

-- Kind
deriving instance
  (Show a) =>
  Show (KindF a)

deriving instance
  (Eq a) =>
  Eq (KindF a)

deriving instance
  (Ord a) =>
  Ord (KindF a)

deriving instance
  (Data a) =>
  Data (KindF a)

deriving instance
  (Typeable a) =>
  Typeable (KindF a)

deriveShow1 ''KindF

deriveEq1 ''KindF

deriveOrd1 ''KindF

deriving instance Functor KindF

deriving instance Foldable KindF

deriving instance Traversable KindF

-- Type
deriving instance
  (Show a) =>
  Show (TypeF a)

deriving instance
  (Eq a) =>
  Eq (TypeF a)

deriving instance
  (Ord a) =>
  Ord (TypeF a)

deriving instance
  (Data a) =>
  Data (TypeF a)

deriving instance
  (Typeable a) =>
  Typeable (TypeF a)

deriveShow1 ''TypeF

deriveEq1 ''TypeF

deriveOrd1 ''TypeF

deriving instance Functor TypeF

deriving instance Foldable TypeF

deriving instance Traversable TypeF
