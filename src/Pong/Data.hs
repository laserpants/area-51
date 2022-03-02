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
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.Ord.Deriving (deriveOrd1)
import Data.Void (Void)
import GHC.Generics (Generic)
import Pong.Util (Fix(..), Name, Text, embed, embed1, embed2, embed3, embed4)
import Text.Show.Deriving (deriveShow1)

data RowF e v a
  = RNil
  | RVar v
  | RExt Name e a

type Row e v = Fix (RowF e v)

data TypeF g a
  = TUnit
  | TBool
  | TInt32
  | TInt64
  | TFloat
  | TDouble
  | TChar
  | TString
  | TCon Name [a]
  | TArr a a
  | TVar Int
  | TGen g
  | TRow (Row (TypeT g) Name)

type TypeT t = Fix (TypeF t)

type Type = TypeT Void

type PolyType = TypeT Int

data TCon
  = VarT
  | ArrT

data Literal
  = LBool Bool
  | LInt32 Int
  | LInt64 Int
  | LFloat Float
  | LDouble Double
  | LChar Char
  | LString Text
  | LUnit

data Op2
  = OEqInt32
  | OAddInt32
  | OSubInt32
  | OMulInt32
  | OAddFloat
  | OMulFloat
  | OSubFloat
  | ODivFloat
  | OAddDouble
  | OMulDouble
  | OSubDouble
  | ODivDouble

type Label t = (t, Name)

data ExprF t a0 a1 a2 a
  = EVar (Label t)
  | ECon (Label t)
  | ELit Literal
  | EIf a a a
  | ELet (Label t) a a
  | EApp a0 a [a]
  | ELam a1 [Label t] a
  | ECall a2 (Label t) [a]
  | EOp2 Op2 a a
  | ECase a [([Label t], a)]
  | ERow (Row (Expr t a0 a1 a2) (Label t))

type Expr t a0 a1 a2 = Fix (ExprF t a0 a1 a2)

type PreAst = Expr Type Type Void Void

type Ast = Expr Type Void Void ()

data Con
  = VarE
  | LitE
  | LamE

newtype Environment a =
  Env
    { getEnvironment :: Map Name a
    }

data Constructor =
  Constructor
    { consName :: Name
    , consFields :: [Type]
    }

data Definition r a
  = Function (NonEmpty r) (Type, a)
  | Constant (Type, a)
  | External [Type] (Label Type)
  | Data Name [Constructor]

newtype Program a = Program { getProgram :: Map Name (Definition (Label Type) a) }

-- Row
deriving instance (Show e, Show v, Show a) => Show (RowF e v a)

deriving instance (Eq e, Eq v, Eq a) => Eq (RowF e v a)

deriving instance (Ord e, Ord v, Ord a) => Ord (RowF e v a)

deriveShow1 ''RowF

deriveEq1 ''RowF

deriveOrd1 ''RowF

deriving instance Functor (RowF e v)

deriving instance Foldable (RowF e v)

deriving instance Traversable (RowF e v)

-- Type
deriving instance (Show g, Show a) => Show (TypeF g a)

deriving instance (Eq g, Eq a) => Eq (TypeF g a)

deriving instance (Ord g, Ord a) => Ord (TypeF g a)

deriveShow1 ''TypeF

deriveEq1 ''TypeF

deriveOrd1 ''TypeF

deriving instance Functor (TypeF g)

deriving instance Foldable (TypeF g)

deriving instance Traversable (TypeF g)

-- TCon
deriving instance Show TCon

deriving instance Eq TCon

deriving instance Ord TCon

-- Con
deriving instance Show Con

deriving instance Eq Con

deriving instance Ord Con

-- Literal
deriving instance Show Literal

deriving instance Eq Literal

deriving instance Ord Literal

-- Op2
deriving instance Show Op2

deriving instance Eq Op2

deriving instance Ord Op2

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
deriving instance (Show r, Show a) => Show (Definition r a)

deriving instance (Eq r, Eq a) => Eq (Definition r a)

deriving instance Functor (Definition r)

deriving instance Foldable (Definition r)

deriving instance Traversable (Definition r)

-- Program

deriving instance (Show a) => Show (Program a)

deriving instance (Eq a) => Eq (Program a)

deriving instance Generic (Program a)

instance Newtype (Program a)
