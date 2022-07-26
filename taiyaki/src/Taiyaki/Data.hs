{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Taiyaki.Data where

import Taiyaki.Util
import Taiyaki.Util.Env (Environment)

-------------------------------------------------------------------------------

data KindF a
  = KTyp
  | KRow
  | KArr a a

type Kind = Fix KindF

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
  | TList a
  | TVar Kind v
  | TCon Kind Name
  | TApp Kind a a
  | TArr a a
  | TRec a
  | TNil
  | TExt Name a a

type Type v = Fix (TypeF v)

-------------------------------------------------------------------------------

-- newtype MonoIndex = MonoIndex Int
-- newtype PolyIndex = PolyIndex Int
--
-- newtype MonoType = MonoType (Type MonoIndex)
-- newtype Generic = Generic (Type PolyIndex)

-------------------------------------------------------------------------------

-- data Predicate a = InClass Name a
--
-- data Scheme
--  = Forall [Kind] [Predicate Generic] Generic

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

data Prim
  = IUnit
  | IBool    Bool
  | IInt     Int
  | IBig     Integer
  | INat     Integer
  | IFloat   Float
  | IDouble  Double
  | IChar    Char
  | IString  Text

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

data PatternF t a
  = PVar  t Name
  | PLit  t Prim
  | PAs   t Name a
  | POr   t a a
  | PAny  t
  | PCon  t Name [a]
  | PTup  t [a]
  | PList t [a]
  | PRec  t a
  | PNil  t
  | PExt  t Name a a
  | PAnn  t a

{- ORMOLU_ENABLE -}

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

{- ORMOLU_DISABLE -}

data Op1 t
  = ONot t
  | ONeg t

-------------------------------------------------------------------------------

data Op2 t
  = OEq   t
  | ONEq  t
  | OLt   t
  | OGt   t
  | OLtE  t
  | OGtE  t
  | OAdd  t
  | OSub  t
  | OMul  t
  | ODiv  t
  | OPow  t
  | OMod  t
  | OOr   t
  | OAnd  t
  | OLArr t
  | ORarr t
  | OFPip t
  | OBPip t
  | ODot  t
  | OGet  t

-------------------------------------------------------------------------------

data ExprF t a
  = EVar  t Name
  | ECon  t Name
  | ELit  t Prim
  | EApp  t a [a]
  | ELam  t [Pattern t] a
  | EIf   t a a a
  | EPat  t a [Clause t]
  | ELet  t (Binding t) a a
  | EFix  t Name a a
  | EFun  t [Clause t]
  | EOp1  t (Op1 t) a
  | EOp2  t (Op2 t) a a
  | ETup  t [a]
  | EList t [a]
  | ERec  t a
  | ENil  t
  | EExt  t Name a a
  | ESub  t
  | ECo   t a
  | EAnn  t a

{- ORMOLU_ENABLE -}

type Expr t = Fix (ExprF t)

-------------------------------------------------------------------------------

type FieldSet a = Map Name [a]

-------------------------------------------------------------------------------

data Assoc
  = ALeft
  | ARight
  | ANone

-------------------------------------------------------------------------------

data PatternGroup t
  = ConGroup Name [Pattern t]
  | OrPattern (Pattern t) (Pattern t)
  | WildcardPattern

type PatternMatrix t = [[Pattern t]]

-------------------------------------------------------------------------------

type ConstructorEnv =
  Environment (Set Name, Int)

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

-- Kind
deriving instance (Show a) =>
  Show (KindF a)

deriving instance (Eq a) =>
  Eq (KindF a)

deriving instance (Ord a) =>
  Ord (KindF a)

deriving instance (Data a) =>
  Data (KindF a)

deriving instance (Typeable a) =>
  Typeable (KindF a)

deriveShow1 ''KindF

deriveEq1 ''KindF

deriveOrd1 ''KindF

deriving instance Functor KindF

deriving instance Foldable KindF

deriving instance Traversable KindF

-- Type
deriving instance (Show v, Show a) =>
  Show (TypeF v a)

deriving instance (Eq v, Eq a) =>
  Eq (TypeF v a)

deriving instance (Ord v, Ord a) =>
  Ord (TypeF v a)

deriving instance (Data v, Data a) =>
  Data (TypeF v a)

deriving instance (Typeable v, Typeable a) =>
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
deriving instance (Show t, Show a) =>
  Show (PatternF t a)

deriving instance (Eq t, Eq a) =>
  Eq (PatternF t a)

deriving instance (Ord t, Ord a) =>
  Ord (PatternF t a)

deriving instance (Data t, Data a) =>
  Data (PatternF t a)

deriving instance (Typeable t, Typeable a) =>
  Typeable (PatternF t a)

deriveShow1 ''PatternF

deriveEq1 ''PatternF

deriveOrd1 ''PatternF

deriving instance Functor (PatternF t)

deriving instance Foldable (PatternF t)

deriving instance Traversable (PatternF t)

-- Binding
deriving instance (Show t) =>
  Show (Binding t)

deriving instance (Eq t) =>
  Eq (Binding t)

deriving instance (Ord t) =>
  Ord (Binding t)

deriving instance (Data t) =>
  Data (Binding t)

deriving instance (Typeable t) =>
  Typeable (Binding t)

-- Choice
deriving instance Show Choice

deriving instance Eq Choice

deriving instance Ord Choice

deriving instance Data Choice

deriving instance Typeable Choice

-- Clause
deriving instance (Show t) =>
  Show (Clause t)

deriving instance (Eq t) =>
  Eq (Clause t)

deriving instance (Ord t) =>
  Ord (Clause t)

deriving instance (Data t) =>
  Data (Clause t)

deriving instance (Typeable t) =>
  Typeable Clause

-- Op1
deriving instance (Show t) =>
  Show (Op1 t)

deriving instance (Eq t) =>
  Eq (Op1 t)

deriving instance (Ord t) =>
  Ord (Op1 t)

deriving instance (Data t) =>
  Data (Op1 t)

deriving instance (Typeable t) =>
  Typeable (Op1 t)

-- Op2
deriving instance (Show t) =>
  Show (Op2 t)

deriving instance (Eq t) =>
  Eq (Op2 t)

deriving instance (Ord t) =>
  Ord (Op2 t)

deriving instance (Data t) =>
  Data (Op2 t)

deriving instance (Typeable t) =>
  Typeable (Op2 t)

-- Expr
deriving instance (Show t, Show a) =>
  Show (ExprF t a)

deriving instance (Eq t, Eq a) =>
  Eq (ExprF t a)

deriving instance (Ord t, Ord a) =>
  Ord (ExprF t a)

deriving instance (Data t, Data a) =>
  Data (ExprF t a)

deriving instance (Typeable t, Typeable a) =>
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

-- PatternGroup
deriving instance (Show t) =>
  Show (PatternGroup t)

deriving instance (Eq t) =>
  Eq (PatternGroup t)

deriving instance (Ord t) =>
  Ord (PatternGroup t)

deriving instance (Data t) =>
  Data (PatternGroup t)

deriving instance (Typeable t) =>
  Typeable (PatternGroup t)
