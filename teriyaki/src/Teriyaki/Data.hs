{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Teriyaki.Data where

import Teriyaki.Util

-------------------------------------------------------------------------------

data KindF a
  = KTyp
  | KRow
  | KArr a a

type Kind = Fix KindF

-------------------------------------------------------------------------------

type TVar v = (Kind, v)

-------------------------------------------------------------------------------

data TypeF v a
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
  | TVar (TVar v)
  | TCon Kind Name
  | TApp Kind a a
  | TArr a a
  | TRec a
  | TNil
  | TExt Name a a

type Type v = Fix (TypeF v)

-------------------------------------------------------------------------------

--newtype MonoIndex = MonoIndex Int
--newtype PolyIndex = PolyIndex Int
--
--newtype MonoType = MonoType (Type MonoIndex)
--newtype Generic = Generic (Type PolyIndex)

-------------------------------------------------------------------------------

--data Predicate a = InClass Name a
--
--data Scheme 
--  = Forall [Kind] [Predicate Generic] Generic

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

type Label t = (t, Name)

-------------------------------------------------------------------------------

data PatternF t a
  = PVar (Label t)
  | PLit Prim
  | PAs a
  | POr a a
  | PAny
  | PCon (Label t) [a]
  | PTup [a]
  | PList [a]
  | PNil
  | PExt Name a a
  | PAnn t a

type Pattern t = Fix (PatternF t)

-------------------------------------------------------------------------------

data Binding t
  = BPat t (Pattern t)
  | BFun t Name [Pattern t]

-------------------------------------------------------------------------------

data Choice
  = Choice

-------------------------------------------------------------------------------

data Clause t
  = Clause t [Pattern t] [Choice]

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

data ExprF t a
  = EVar (Label t)
  | ECon (Label t)
  | ELit Prim
  | EApp t a [a]
  | ELam t [Pattern t] a
  | EIf a a a
  | EPat -- ?
  | ELet (Binding t) a a
  | EFix (Label t) a a
  | EFun -- ?
  | EOp1 (t, Op1) a
  | EOp2 (t, Op2) a a
  | ETup
  | EList
  | ENil
  | EExt Name a a
  | ESub t

--  | EAnn Type a

--  | ECo a

type Expr t = Fix (ExprF t)

-------------------------------------------------------------------------------

data Assoc
  = ALeft
  | ARight
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
  (Show v, Show a) =>
  Show (TypeF v a)

deriving instance
  (Eq v, Eq a) =>
  Eq (TypeF v a)

deriving instance
  (Ord v, Ord a) =>
  Ord (TypeF v a)

deriving instance
  (Data v, Data a) =>
  Data (TypeF v a)

deriving instance
  (Typeable v, Typeable a) =>
  Typeable (TypeF v a)

deriveShow1 ''TypeF

deriveEq1 ''TypeF

deriveOrd1 ''TypeF

deriving instance Functor (TypeF v)

deriving instance Foldable (TypeF v)

deriving instance Traversable (TypeF v)

-- Prim
deriving instance Show Prim

deriving instance Eq Prim

deriving instance Ord Prim

deriving instance Data Prim

deriving instance Typeable Prim

-- Pattern
deriving instance
  (Show t, Show a) =>
  Show (PatternF t a)

deriving instance
  (Eq t, Eq a) =>
  Eq (PatternF t a)

deriving instance
  (Ord t, Ord a) =>
  Ord (PatternF t a)

deriving instance
  (Data t, Data a) =>
  Data (PatternF t a)

deriving instance
  (Typeable t, Typeable a) =>
  Typeable (PatternF t a)

deriveShow1 ''PatternF

deriveEq1 ''PatternF

deriveOrd1 ''PatternF

deriving instance Functor (PatternF t)

deriving instance Foldable (PatternF t)

deriving instance Traversable (PatternF t)

-- Binding
deriving instance (Show t) => Show (Binding t)

deriving instance (Eq t) => Eq (Binding t)

deriving instance (Ord t) => Ord (Binding t)

deriving instance (Data t) => Data (Binding t)

deriving instance (Typeable t) => Typeable (Binding t)

-- Choice
deriving instance Show Choice

deriving instance Eq Choice

deriving instance Ord Choice

deriving instance Data Choice

deriving instance Typeable Choice

-- Clause
deriving instance (Show t) => Show (Clause t)

deriving instance (Eq t) => Eq (Clause t)

deriving instance (Ord t) => Ord (Clause t)

deriving instance (Data t) => Data (Clause t)

deriving instance (Typeable t) => Typeable Clause

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
  (Show t, Show a) =>
  Show (ExprF t a)

deriving instance
  (Eq t, Eq a) =>
  Eq (ExprF t a)

deriving instance
  (Ord t, Ord a) =>
  Ord (ExprF t a)

deriving instance
  (Data t, Data a) =>
  Data (ExprF t a)

deriving instance
  (Typeable t, Typeable a) =>
  Typeable (ExprF t a)

deriveShow1 ''ExprF

deriveEq1 ''ExprF

deriveOrd1 ''ExprF

deriving instance Functor (ExprF t)

deriving instance Foldable (ExprF t)

deriving instance Traversable (ExprF t)

-- Assoc
deriving instance Show Assoc

deriving instance Eq Assoc

deriving instance Ord Assoc

deriving instance Data Assoc

deriving instance Typeable Assoc
