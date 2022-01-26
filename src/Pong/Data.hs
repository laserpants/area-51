{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Pong.Data where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Eq.Deriving (deriveEq1)
import Data.Map.Strict (Map)
import Data.Ord.Deriving (deriveOrd1)
import LLVM.AST.Operand
import LLVM.IRBuilder
import Pong.Util
  ( Fix
  , Name
  , Names
  , Text
  , embed
  , embed1
  , embed2
  , embed3
  , project
  )
import Text.Show.Deriving (deriveShow1)

data TypeF a
  = TUnit
  | TBool
  | TInt32
  | TInt64
  | TFloat
  | TDouble
  | TChar
  | TString
  | TArr a a
  | TVar Int
  | TData Name
  | TOpaque

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

type TyId t = (t, Name)

data ExprF t a
  = EVar (TyId t)
  | ELit Literal
  | EIf a a a
  | ELam [TyId t] a
  | ELet (TyId t) a a
  | EApp t a [a]
  | ECall (TyId t) [a]
  | EOp2 Op2 a a
  | ECase a [([TyId t], a)]

type Expr t = Fix (ExprF t)

type Ast = Expr Type

data Con
  = VarE
  | LitE
  | LamE

--data BodyF a
--  = BVar Name
--  | BLit Literal
--  | BIf a a a
--  | BCall Name [a]
--  | BOp2 Op2 a a
--  | BCase a [(Names, a)]

type Body = Ast
--type Body = Fix BodyF

data Clause a =
  Clause [a] [a]

newtype Environment a =
  Env
    { getEnvironment :: Map Name a
    }

type TypeEnv = Environment Type

data Signature a =
  Signature
    { arguments :: [(Type, Name)]
    , body :: (Type, a)
    }

data Constructor =
  Constructor
    { consName :: Name
    , consFields :: [Type]
    }

data Definition a
  = Function (Signature a) -- ^ Function definition
  | External (Signature ()) -- ^ External declaration
  | Constant Literal -- ^ Constant value
  | Data Name [Constructor] -- ^ Data type definition

data Program =
  Program
    { count :: Int
    , definitions :: Map Name (Definition Body)
    }

newtype Substitution =
  Substitution
    { getSubstitution :: Map Int Type
    }

newtype TypeChecker a =
  TypeChecker
    { getTypeChecker :: ExceptT TypeError (ReaderT TypeEnv (State (Int, Substitution))) a
    }

data TypeError
  = UnificationError
  | NotInScope Name
  | EmptyCaseStatement

newtype Compiler a =
  Compiler
    { getCompiler :: ReaderT TypeEnv (State Program) a
    }

type CodeGenEnv = Environment Operand

newtype CodeGen a =
  CodeGen
    { getCodeGen :: ReaderT CodeGenEnv (IRBuilderT ModuleBuilder) a
    }

class Source a where
  toProgram :: [(Name, Definition a)] -> Program -- TODO: Either Error Program

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

-- Lit
deriving instance Show Literal

deriving instance Eq Literal

deriving instance Ord Literal

-- Op2
deriving instance Show Op2

deriving instance Eq Op2

deriving instance Ord Op2

-- Expr
deriving instance (Show t, Show a) => Show (ExprF t a)

deriving instance (Eq t, Eq a) => Eq (ExprF t a)

deriving instance (Ord t, Ord a) => Ord (ExprF t a)

deriveShow1 ''ExprF

deriveEq1 ''ExprF

deriveOrd1 ''ExprF

deriving instance Functor (ExprF t)

deriving instance Foldable (ExprF t)

deriving instance Traversable (ExprF t)

-- Con
deriving instance Show Con

deriving instance Eq Con

deriving instance Ord Con

---- Body
--deriving instance (Show a) => Show (BodyF a)
--
--deriving instance (Eq a) => Eq (BodyF a)
--
--deriving instance (Ord a) => Ord (BodyF a)
--
--deriveShow1 ''BodyF
--
--deriveEq1 ''BodyF
--
--deriveOrd1 ''BodyF
--
--deriving instance Functor BodyF
--
--deriving instance Foldable BodyF
--
--deriving instance Traversable BodyF

-- Clause
deriving instance (Show a) => Show (Clause a)

deriving instance (Eq a) => Eq (Clause a)

deriving instance (Ord a) => Ord (Clause a)

-- Environment
deriving instance (Show a) => Show (Environment a)

deriving instance (Eq a) => Eq (Environment a)

deriving instance (Ord a) => Ord (Environment a)

deriving instance Functor Environment

instance Semigroup (Environment a) where
  Env a <> Env b = Env (a <> b)

instance Monoid (Environment a) where
  mempty = Env mempty

-- Signature
deriving instance (Show a) => Show (Signature a)

deriving instance (Eq a) => Eq (Signature a)

deriving instance Functor Signature

deriving instance Foldable Signature

deriving instance Traversable Signature

-- Definition
deriving instance (Show a) => Show (Definition a)

deriving instance (Eq a) => Eq (Definition a)

deriving instance Functor Definition

deriving instance Foldable Definition

deriving instance Traversable Definition

-- Program
deriving instance Show Program

deriving instance Eq Program

-- Substitution
deriving instance Show Substitution

deriving instance Eq Substitution

deriving instance Ord Substitution

-- TypeChecker
deriving instance Functor TypeChecker

deriving instance Applicative TypeChecker

deriving instance Monad TypeChecker

deriving instance (MonadState (Int, Substitution)) TypeChecker

deriving instance (MonadReader TypeEnv) TypeChecker

deriving instance (MonadError TypeError) TypeChecker

-- TypeError
deriving instance Show TypeError

deriving instance Eq TypeError

-- Compiler
deriving instance Functor Compiler

deriving instance Applicative Compiler

deriving instance Monad Compiler

deriving instance (MonadState Program) Compiler

deriving instance (MonadReader TypeEnv) Compiler

-- Constructor
deriving instance Show Constructor

deriving instance Eq Constructor

deriving instance Ord Constructor

-- CodeGen
deriving instance Functor CodeGen

deriving instance Applicative CodeGen

deriving instance Monad CodeGen

deriving instance (MonadReader CodeGenEnv) CodeGen

deriving instance MonadFix CodeGen

deriving instance MonadIRBuilder CodeGen

deriving instance MonadModuleBuilder CodeGen
