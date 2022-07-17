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

data Binding
  = BPat Type Pattern
  | BFun Type Name [Pattern]

-------------------------------------------------------------------------------

data Clause
  = Clause

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

data ExprF a
  = EVar Label
  | ECon Label
  | ELit Prim
  | EApp Type a [a]
  | ELam Type [Pattern] a
  | EIf a a a
  | EPat -- ?
  | ELet Binding a a
  | EFix Label a a
  | EFun -- ?
  | EOp1 (Type, Op1) a
  | EOp2 (Type, Op2) a a
  | ETup
  | EList
  | ENil
  | EExt Name a a
  | EAnn Type a
  | EHole Type

-- TODO:
-- Codata?

type Expr = Fix ExprF

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

-- Prim
deriving instance Show Prim

deriving instance Eq Prim

deriving instance Ord Prim

deriving instance Data Prim

deriving instance Typeable Prim

-- Pattern
deriving instance
  (Show a) =>
  Show (PatternF a)

deriving instance
  (Eq a) =>
  Eq (PatternF a)

deriving instance
  (Ord a) =>
  Ord (PatternF a)

deriving instance
  (Data a) =>
  Data (PatternF a)

deriving instance
  (Typeable a) =>
  Typeable (PatternF a)

deriveShow1 ''PatternF

deriveEq1 ''PatternF

deriveOrd1 ''PatternF

deriving instance Functor PatternF

deriving instance Foldable PatternF

deriving instance Traversable PatternF

-- Binding
deriving instance Show Binding

deriving instance Eq Binding

deriving instance Ord Binding

deriving instance Data Binding

deriving instance Typeable Binding

-- Clause
deriving instance Show Clause

deriving instance Eq Clause

deriving instance Ord Clause

deriving instance Data Clause

deriving instance Typeable Clause

-- Op1
deriving instance Show Op1

deriving instance Eq Op1

deriving instance Ord Op1

deriving instance Data Op1

deriving instance Typeable Op1

-- Op2
deriving instance Show Op2

deriving instance Eq Op2

deriving instance Ord Op2

deriving instance Data Op2

deriving instance Typeable Op2

-- Expr
deriving instance
  (Show a) =>
  Show (ExprF a)

deriving instance
  (Eq a) =>
  Eq (ExprF a)

deriving instance
  (Ord a) =>
  Ord (ExprF a)

deriving instance
  (Data a) =>
  Data (ExprF a)

deriving instance
  (Typeable a) =>
  Typeable (ExprF a)

deriveShow1 ''ExprF

deriveEq1 ''ExprF

deriveOrd1 ''ExprF

deriving instance Functor ExprF

deriving instance Foldable ExprF

deriving instance Traversable ExprF

-- Assoc
deriving instance Show Assoc

deriving instance Eq Assoc

deriving instance Ord Assoc

deriving instance Data Assoc

deriving instance Typeable Assoc
