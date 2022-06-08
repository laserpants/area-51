{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Pong.Data where

import Control.Newtype.Generics (Newtype)
import Data.Data (Data)
import Data.Map.Strict (Map)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Pong.Util (Fix (..), List1, Name, Text, Void, deriveEq1, deriveOrd1, deriveShow1)

{- ORMOLU_DISABLE -}

data RowF e r a
  = RNil                           -- ^ Empty row
  | RVar r                         -- ^ Row variable
  | RExt Name e a                  -- ^ Row extension

{- ORMOLU_ENABLE -}

-- | A row is a sequence of labeled fields. Rows encode the internal structure
-- of records, both at the type and expression level. A row can be either
-- /open/ or /closed/. An open row has a variable in the tail of the sequence,
-- whereas a closed row is one that ends with the empty row.
type Row e r = Fix (RowF e r)

{- ORMOLU_DISABLE -}

data TypeF v s a
  = TUnit                          -- ^ Unit type
  | TBool                          -- ^ Boolean type
  | TInt                           -- ^ Type of integers (machine bounded)
  | TFloat                         -- ^ Single precision floating point number
  | TDouble                        -- ^ Double precision floating point number
  | TChar                          -- ^ Char type
  | TString                        -- ^ Unicode strings
  | TCon Name [a]                  -- ^ Algebraic data-types
  | TArr a a                       -- ^ Function types
  | TVar v                         -- ^ Type variable (monomorphic)
  | TGen s                         -- ^ Quantified type variable
  | TRec (Row (Type v s) Int)      -- ^ Record type

{- ORMOLU_ENABLE -}

-- | Parameterized type representation
type Type v s = Fix (TypeF v s)

-- | Monomorphic type
type MonoType = Type Int Void

-- | Polymorphic type scheme
newtype Scheme = Scheme (Type Void Name)

{- ORMOLU_DISABLE -}

-- | Type variety (head construtor of a Type value)
data ConT
  = VarT                           -- ^ Type is a TVar
  | ArrT                           -- ^ Type is a TArr
  | RecT                           -- ^ Type is a TRec

-- | Built-in language primitives
data Prim
  = PBool Bool                     -- ^ Booleans
  | PInt Int                       -- ^ Integers (machine bounded)
  | PFloat Float                   -- ^ Single-precision floating point number
  | PDouble Double                 -- ^ Double-precision floating point number
  | PChar Char                     -- ^ Char
  | PString Text                   -- ^ Unicode strings
  | PUnit                          -- ^ Unit value

-- | Unary operators
data Op1
  = ONot                           -- ^ Logical NOT
  | ONeg                           -- ^ Negation

-- | Binary operators
data Op2
  = OEq                            -- ^ Equal to
  | ONEq                           -- ^ Not equal to
  | OLt                            -- ^ Less than
  | OGt                            -- ^ Greater than
  | OLtE                           -- ^ Less than or equal to
  | OGtE                           -- ^ Greater than or equal to
  | OAdd                           -- ^ Addition
  | OSub                           -- ^ Subtraction
  | OMul                           -- ^ Multiplication
  | ODiv                           -- ^ Division
  | OLogicOr                       -- ^ Logical OR
  | OLogicAnd                      -- ^ Logical AND

{- ORMOLU_ENABLE -}

-- | A label is a typed identifier
type Label t = (t, Name)

-- | Match expression clause
type Clause t a = ([Label t], a)

{- ORMOLU_DISABLE -}

data ExprF t a0 a1 a2 a
  = EVar (Label t)                           -- ^ Variable
  | ECon (Label a0)                          -- ^ Data constructor
  | ELit Prim                                -- ^ Literal
  | EIf a a a                                -- ^ If statement
  | ELet (Label t) a a                       -- ^ Let binding
  | EApp a0 a [a]                            -- ^ Application
  | ELam a1 [Label t] a                      -- ^ Lambda abstraction
  | ECall a2 (Label t) [a]                   -- ^ Function call
  | EOp1 (t, Op1) a                          -- ^ Unary operator
  | EOp2 (t, Op2) a a                        -- ^ Binary operator
  | EPat a [Clause t a]                      -- ^ Match statement
  | ERec (Row (Expr t a0 a1 a2) (Label t))   -- ^ Record
  | ERes [Label t] a a                       -- ^ Field restriction operator

{- ORMOLU_ENABLE -}

-- | Parameterized expression grammar
type Expr t a0 a1 a2 = Fix (ExprF t a0 a1 a2)

-- | Untyped source expression
type SourceExpr = Expr () () () Void

-- | Source expression annotated with numeric tags to support type inference
type TaggedExpr = Expr Int Int () Void

-- | Type annotated source expression
type TypedExpr = Expr MonoType MonoType () Void

-- | Typed intermediate representation
type PreAst = Expr MonoType MonoType Void Void

-- | Translated expression
type Ast = Expr MonoType Void Void ()

{- ORMOLU_DISABLE -}

-- | Expression variety (head construtor of an Expr value)
data ConE
  = VarE                           -- ^ Expression is an EVar
  | LitE                           -- ^ Expression is an ELit
  | LamE                           -- ^ Expression is an ELam
  | RecE                           -- ^ Expression is an ERec

{- ORMOLU_ENABLE -}

data Constructor t = Constructor
  { conName :: Name
  , conFields :: [t]
  }

data Definition t a
  = Function (List1 (Label t)) (t, a)
  | Constant (t, a)
  | Extern [MonoType] MonoType
  | Data Name [Constructor t]

newtype Program t a
  = Program (Map (Label Scheme) (Definition t a))

-------------------------------------------------------------------------------
-- Typeclass instances
-------------------------------------------------------------------------------

-- Row
deriving instance (Show e, Show r, Show a) => Show (RowF e r a)

deriving instance (Eq e, Eq r, Eq a) => Eq (RowF e r a)

deriving instance (Ord e, Ord r, Ord a) => Ord (RowF e r a)

deriving instance (Data e, Data r, Data a) => Data (RowF e r a)

deriving instance (Typeable e, Typeable r, Typeable a) => Typeable (RowF e r a)

deriveShow1 ''RowF

deriveEq1 ''RowF

deriveOrd1 ''RowF

deriving instance Functor (RowF e r)

deriving instance Foldable (RowF e r)

deriving instance Traversable (RowF e r)

-- Type
deriving instance (Show v, Show s, Show a) => Show (TypeF v s a)

deriving instance (Eq v, Eq s, Eq a) => Eq (TypeF v s a)

deriving instance (Ord v, Ord s, Ord a) => Ord (TypeF v s a)

deriving instance (Data v, Data s, Data a) => Data (TypeF v s a)

deriving instance (Typeable v, Typeable s, Typeable a) => Typeable (TypeF v s a)

deriveShow1 ''TypeF

deriveEq1 ''TypeF

deriveOrd1 ''TypeF

deriving instance Functor (TypeF v s)

deriving instance Foldable (TypeF v s)

deriving instance Traversable (TypeF v s)

-- Scheme
deriving instance Show Scheme

deriving instance Eq Scheme

deriving instance Ord Scheme

deriving instance Generic Scheme

instance Newtype Scheme

-- TCon
deriving instance Show ConT

deriving instance Eq ConT

deriving instance Ord ConT

-- Con
deriving instance Show ConE

deriving instance Eq ConE

deriving instance Ord ConE

-- Prim
deriving instance Show Prim

deriving instance Eq Prim

deriving instance Ord Prim

deriving instance Data Prim

deriving instance Typeable Prim

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
  (Show t, Show a0, Show a1, Show a2, Show a) =>
  Show (ExprF t a0 a1 a2 a)

deriving instance
  (Eq t, Eq a0, Eq a1, Eq a2, Eq a) =>
  Eq (ExprF t a0 a1 a2 a)

deriving instance
  (Ord t, Ord a0, Ord a1, Ord a2, Ord a) =>
  Ord (ExprF t a0 a1 a2 a)

deriving instance
  (Data t, Data a0, Data a1, Data a2, Data a) =>
  Data (ExprF t a0 a1 a2 a)

deriving instance
  (Typeable t, Typeable a0, Typeable a1, Typeable a2, Typeable a) =>
  Typeable (ExprF t a0 a1 a2 a)

deriving instance
  (Typeable t) =>
  Typeable (ExprF t a0 a1 a2 a)

deriveShow1 ''ExprF

deriveEq1 ''ExprF

deriveOrd1 ''ExprF

deriving instance Functor (ExprF t a0 a1 a2)

deriving instance Foldable (ExprF t a0 a1 a2)

deriving instance Traversable (ExprF t a0 a1 a2)

-- Constructor
deriving instance (Show t) => Show (Constructor t)

deriving instance (Eq t) => Eq (Constructor t)

deriving instance (Ord t) => Ord (Constructor t)

deriving instance Functor Constructor

deriving instance Foldable Constructor

deriving instance Traversable Constructor

-- Definition
deriving instance (Show t, Show a) => Show (Definition t a)

deriving instance (Eq t, Eq a) => Eq (Definition t a)

deriving instance Functor (Definition t)

deriving instance Foldable (Definition t)

deriving instance Traversable (Definition t)

-- Program
deriving instance (Show t, Show a) => Show (Program t a)

deriving instance (Eq t, Eq a) => Eq (Program t a)

deriving instance (Ord t) => Semigroup (Program t a)

deriving instance Generic (Program t a)

instance Newtype (Program t a)

deriving instance Functor (Program t)

deriving instance Foldable (Program t)

deriving instance Traversable (Program t)
