{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Pong.Data where

import Data.Eq.Deriving (deriveEq1)
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.Ord.Deriving (deriveOrd1)
import Data.Void (Void)
import Pong.Util (Fix(..), Name, Text, embed, embed1, embed2, embed3, embed4)
import Text.Show.Deriving (deriveShow1)

data RowF r a
  = RNil
  | RVar Name
  | RExt Name r a

type Row r = Fix (RowF r)

data TypeF a
  = TUnit
  | TBool
  | TInt32
  | TInt64
  | TFloat
  | TDouble
  | TChar
  | TString
  | TCon Name
  | TArr a a
  | TVar Int
  | TGen Int
  | TRow (Row Type)

type Type = Fix TypeF

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
  | ELam a0 [Label t] a
  | EApp a1 a [a]
  | ECall a2 (Label t) [a]
  | EOp2 Op2 a a
  | ECase a [([Label t], a)]
  | ERow (Row (Expr t a0 a1 a2))

type Expr t a0 a1 a2 = Fix (ExprF t a0 a1 a2)

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
  | External [Type]
  | Data Name [Constructor]

-- Row
deriving instance (Show r, Show a) => Show (RowF r a)

deriving instance (Eq r, Eq a) => Eq (RowF r a)

deriving instance (Ord r, Ord a) => Ord (RowF r a)

deriveShow1 ''RowF

deriveEq1 ''RowF

deriveOrd1 ''RowF

deriving instance Functor (RowF r)

deriving instance Foldable (RowF r)

deriving instance Traversable (RowF r)

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

instance Semigroup (Environment a) where
  Env a <> Env b = Env (a <> b)

instance Monoid (Environment a) where
  mempty = Env mempty

-- Definition
deriving instance (Show r, Show a) => Show (Definition r a)

deriving instance (Eq r, Eq a) => Eq (Definition r a)

deriving instance Functor (Definition r)

deriving instance Foldable (Definition r)

deriving instance Traversable (Definition r)
