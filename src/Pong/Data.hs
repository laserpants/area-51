{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Pong.Data where

import Control.Newtype.Generics
import Data.Eq.Deriving (deriveEq1)
import Data.Map.Strict (Map)
import Data.Ord.Deriving (deriveOrd1)
import GHC.Generics (Generic)
import Pong.Util (Void, Name, Text, Fix(..), List1)
import Text.Show.Deriving (deriveShow1)

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

data TCon
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
  = EVar (Label t)
  | ECon (Label a0)
  | ELit Prim
  | EIf a a a
  | ELet (Label t) a a
  | EApp a0 a [a]
  | ELam a1 [Label t] a
  | ECall a2 (Label t) [a]
  | EOp1 (t, Op1) a 
  | EOp2 (t, Op2) a a
  | ECase a [([Label t], a)]
  | ERow (Row (Expr t a0 a1 a2) (Label t))
  | EField [Label t] a a

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

data Con
  = VarE
  | LitE
  | LamE
  | RowE

-- data Constructor =
--   Constructor
--     { conName :: Name
--     , conFields :: [Type]
--     }

data Definition t a
  = Function (List1 (Label t)) (t, a)
--   | Constant (Type, a)
--   | External [Type] (Label Type)
--   | Data Name [Constructor]

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
deriving instance Show TCon

deriving instance Eq TCon

deriving instance Ord TCon

-- Con
deriving instance Show Con

deriving instance Eq Con

deriving instance Ord Con

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

-- -- Constructor
-- deriving instance Show Constructor

-- deriving instance Eq Constructor

-- deriving instance Ord Constructor

-- Definition
deriving instance (Show d, Show a) => Show (Definition d a)

deriving instance (Eq d, Eq a) => Eq (Definition d a)

deriving instance Functor (Definition d)

deriving instance Foldable (Definition d)

deriving instance Traversable (Definition d)

-- Program
deriving instance (Show t, Show a) => Show (Program t a)

deriving instance (Eq t, Eq a) => Eq (Program t a)

deriving instance Generic (Program t a)

instance Newtype (Program t a)
