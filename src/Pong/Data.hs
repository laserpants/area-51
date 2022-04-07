{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Pong.Data where

import Control.Newtype.Generics
import Data.Eq.Deriving (deriveEq1)
import Data.List.NonEmpty (fromList)
import Data.Map.Strict (Map)
import Data.Ord.Deriving (deriveOrd1)
import Data.Void (Void)
import GHC.Generics (Generic)
import Pong.Util
  ( Fix(..)
  , List1
  , Name
  , Text
  , embed
  , embed1
  , embed2
  , embed3
  , embed4
  )
import Text.Show.Deriving (deriveShow1)

data RowF e r a
  = RNil
  | RVar r
  | RExt Name e a

-- | A row is a sequence of labeled fields. Rows encode the internal structure 
-- of records, both at the type and expression level. A row can be either open 
-- or closed. An open row is one that has a variable in the tail of the 
-- sequence, whereas a closed row ends with the empty row.
type Row e r = Fix (RowF e r)

data TypeF a
  = TUnit
  | TBool
  | TInt
  | TFloat
  | TDouble
  | TChar
  | TString
  | TCon Name [a]
  | TArr a a
  | TVar Int
  | TGen Name
  | TRow (Row Type Int)

type Type = Fix TypeF 

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

-- | Binary operators
data Binop
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

-- | Typed operator 
data Op2 t =
  Op2 Binop t

data ExprF t a0 a1 a2 a
  = EVar (Label t)
  | ECon (Label a0)
  | ELit Prim
  | EIf a a a
  | ELet (Label t) a a
  | EApp a0 a [a]
  | ELam a1 [Label t] a
  | ECall a2 (Label t) [a]
  | EOp2 (Op2 t) a a
  | ECase a [([Label t], a)]
  | ERow (Row (Expr t a0 a1 a2) (Label t))
  | EField [Label t] a a

-- | Parameterized main expression language grammar
type Expr t a0 a1 a2 = Fix (ExprF t a0 a1 a2)

-- | Source expression annotated with numeric tags to facilitate type checking
type TaggedExpr = Expr Int Int () Void

-- | Typed source expression
type TypedExpr = Expr Type Type () Void

-- | Typed intermediate representation
type PreAst = Expr Type Type Void Void

type Ast = Expr Type Void Void ()

data Con
  = VarE
  | LitE
  | LamE
  | RowE

newtype Environment a =
  Environment (Map Name a)

data Constructor =
  Constructor
    { conName :: Name
    , conFields :: [Type]
    }

data Definition d a
  = Function (List1 d) (Type, a)
  | Constant (Type, a)
  | External [Type] (Label Type)
  | Data Name [Constructor]

newtype Program a =
  Program (Map Name (Definition (Label Type) a))

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
deriving instance (Show a) => Show (TypeF a)

deriving instance (Eq a) => Eq (TypeF a)

deriving instance (Ord a) => Ord (TypeF a)

deriveShow1 ''TypeF

deriveEq1 ''TypeF

deriveOrd1 ''TypeF

deriving instance Functor TypeF 

deriving instance Foldable TypeF 

deriving instance Traversable TypeF 

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

-- Binop
deriving instance Show Binop

deriving instance Eq Binop

deriving instance Ord Binop

-- Op2
deriving instance (Show t) => Show (Op2 t)

deriving instance (Eq t) => Eq (Op2 t)

deriving instance (Ord t) => Ord (Op2 t)

-- Expr
deriving instance
         (Show t, Show a0, Show a1, Show a2, Show a) =>
         Show (ExprF t a0 a1 a2 a)

deriving instance
         (Eq t, Eq a0, Eq a1, Eq a2, Eq a) => Eq (ExprF t a0 a1 a2 a)

deriving instance
         (Ord t, Ord a0, Ord a1, Ord a2, Ord a) => Ord (ExprF t a0 a1 a2 a)

deriveShow1 ''ExprF

deriveEq1 ''ExprF

deriveOrd1 ''ExprF

deriving instance Functor (ExprF t a0 a1 a2)

deriving instance Foldable (ExprF t a0 a1 a2)

deriving instance Traversable (ExprF t a0 a1 a2)

-- Constructor
deriving instance Show Constructor

deriving instance Eq Constructor

deriving instance Ord Constructor

-- Environment
deriving instance (Show a) => Show (Environment a)

deriving instance (Eq a) => Eq (Environment a)

deriving instance (Ord a) => Ord (Environment a)

deriving instance Functor Environment

deriving instance Foldable Environment

deriving instance Traversable Environment

deriving instance Semigroup (Environment a)

deriving instance Monoid (Environment a)

deriving instance Generic (Environment a)

instance Newtype (Environment a)

-- Definition
deriving instance (Show d, Show a) => Show (Definition d a)

deriving instance (Eq d, Eq a) => Eq (Definition d a)

deriving instance Functor (Definition d)

deriving instance Foldable (Definition d)

deriving instance Traversable (Definition d)

-- Program
deriving instance (Show a) => Show (Program a)

deriving instance (Eq a) => Eq (Program a)

deriving instance Generic (Program a)

instance Newtype (Program a)
