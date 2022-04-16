{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Pong.Data where

import Control.Newtype.Generics (Newtype)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Pong.Util (Void, Name, Text, Fix(..), List1, deriveShow1, deriveEq1, deriveOrd1)

data RowF e r a
  = RNil
  | RVar r
  | RExt Name e a

-- | A row is a sequence of labeled fields which encode the internal structure 
-- of records, both at the type and expression level. A row can be either open 
-- or closed. An open row is one that has a variable in the tail of the 
-- sequence, whereas a closed row ends with the empty row.
type Row e r = Fix (RowF e r)

data TypeF v g a
  = TUnit
  | TBool
  | TInt
  | TFloat
  | TDouble
  | TChar
  | TString
  | TCon Name [a]
  | TArr a a
  | TVar v
  | TGen g
  | TRow (Row (Type v g) Int)

type Type v g = Fix (TypeF v g)

type MonoType = Type Int Void

data ConT
  = VarT
  | ArrT
  | RowT

-- | Built-in language primitives
data Prim
  = PBool Bool
  | PInt Int
  | PFloat Float
  | PDouble Double
  | PChar Char
  | PString Text
  | PUnit

-- | Unary operators
data Op1
  = ONot       -- ^ Logical NOT
  | ONeg       -- ^ Negation

-- | Binary operators
data Op2
  = OEq        -- ^ Equality 
  | OLt        -- ^ Less than
  | OGt        -- ^ Greater than
  | OLtE       -- ^ Less than or equal
  | OGtE       -- ^ Greater than or equal
  | OAdd       -- ^ Addition
  | OSub       -- ^ Subtraction
  | OMul       -- ^ Multiplication
  | ODiv       -- ^ Division
  | OLogicOr   -- ^ Logical OR
  | OLogicAnd  -- ^ Logical AND

-- | A label is a typed identifier 
type Label t = (t, Name)

data ExprF t a0 a1 a2 a
  = EVar (Label t)                           -- ^ Variable
  | ECon (Label a0)                          -- ^ Constructor application
  | ELit Prim                                -- ^ Literal
  | EIf a a a                                -- ^ If statement
  | ELet (Label t) a a                       -- ^ Let-binding
  | EApp a0 a [a]                            -- ^ Application
  | ELam a1 [Label t] a                      -- ^ Lambda function
  | ECall a2 (Label t) [a]                   -- ^ Function call
  | EOp1 (t, Op1) a                          -- ^ Unary operator
  | EOp2 (t, Op2) a a                        -- ^ Binary operator
  | ECase a [([Label t], a)]                 -- ^ Match statement
  | ERow (Row (Expr t a0 a1 a2) (Label t))   -- ^ Row expression
  | EField [Label t] a a                     -- ^ Field accessor

-- | Parameterized main expression language grammar
type Expr t a0 a1 a2 = Fix (ExprF t a0 a1 a2)

-- | Source expression annotated with numeric tags to facilitate type checking
type TaggedExpr = Expr Int Int () Void

-- | Typed source expression
type TypedExpr = Expr MonoType MonoType () Void

-- | Typed intermediate representation
type PreAst = Expr MonoType MonoType Void Void

-- | Translated expression
type Ast = Expr MonoType Void Void ()

data ConE
  = VarE
  | LitE
  | LamE
  | RowE

data Constructor t =
  Constructor
    { conName :: Name
    , conFields :: [t]
    }

data Definition t a
  = Function (List1 (Label t)) (t, a)
  | Data Name [Constructor t]
--   | Constant (Type, a)
--   | External [Type] (Label Type)

newtype Program t a =
  Program (Map Name (Definition t a))

-- Row
deriving instance (Show e, Show r, Show a) => Show (RowF e r a)

deriving instance (Eq e, Eq r, Eq a) => Eq (RowF e r a)

deriving instance (Ord e, Ord r, Ord a) => Ord (RowF e r a)

deriveShow1 ''RowF

deriveEq1 ''RowF

deriveOrd1 ''RowF

deriving instance Functor (RowF e r)

deriving instance Foldable (RowF e r)

deriving instance Traversable (RowF e r)

-- Type
deriving instance (Show v, Show g, Show a) => Show (TypeF v g a)

deriving instance (Eq v, Eq g, Eq a) => Eq (TypeF v g a)

deriving instance (Ord v, Ord g, Ord a) => Ord (TypeF v g a)

deriveShow1 ''TypeF

deriveEq1 ''TypeF

deriveOrd1 ''TypeF

deriving instance Functor (TypeF v g)

deriving instance Foldable (TypeF v g)

deriving instance Traversable (TypeF v g)

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

-- Op1
deriving instance Show Op1

deriving instance Eq Op1

deriving instance Ord Op1

-- Op2
deriving instance Show Op2

deriving instance Eq Op2

deriving instance Ord Op2

-- Expr
deriving instance (Show t, Show a0, Show a1, Show a2, Show a) => Show (ExprF t a0 a1 a2 a)

deriving instance (Eq t, Eq a0, Eq a1, Eq a2, Eq a) => Eq (ExprF t a0 a1 a2 a)

deriving instance (Ord t, Ord a0, Ord a1, Ord a2, Ord a) => Ord (ExprF t a0 a1 a2 a)

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

-- Definition
deriving instance (Show t, Show a) => Show (Definition t a)

deriving instance (Eq t, Eq a) => Eq (Definition t a)

deriving instance Functor (Definition t)

deriving instance Foldable (Definition t)

deriving instance Traversable (Definition t)

-- Program
deriving instance (Show t, Show a) => Show (Program t a)

deriving instance (Eq t, Eq a) => Eq (Program t a)

deriving instance Generic (Program t a)

instance Newtype (Program t a)
