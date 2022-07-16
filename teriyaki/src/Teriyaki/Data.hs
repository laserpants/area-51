{-# LANGUAGE StrictData #-}

module Teriyaki.Data where

import Data.Text (Text)
import Teriyaki.Util

-------------------------------------------------------------------------------

data KindF a
  = KCon Name
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
  | TVar Kind Name
  | TCon Kind Name
  | TApp Kind a a
  | TArr a a
  | TRec a
  | TTup
  | TList
  | TNil
  | TExt Name a a

type Type = Fix TypeF

-------------------------------------------------------------------------------

data Prim
  = PUnit
  | PBool Bool
  | PInt Int
  | PBig Integer
  | PNat Integer
  | PFloat Float
  | PDouble Double
  | PChar Char
  | PString Text

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

data ExprF a
  = EVar Label
  | ECon Label
  | ELit Prim
  | EApp Type a [a]
  | EFix -- ?
  | ELam -- ?
  | EIf a a a
  | EPat -- ?
  | ELet Label a a
  | EFun -- ?
  | EOp1 (Type, Op1) a
  | EOp2 (Type, Op2) a a
  | ETup
  | EList
  | ENil
  | EExt Name a a
  | EAnn Type a

-- TODO:
-- Codata?
-- Hole?

type Expr = Fix ExprF

-------------------------------------------------------------------------------

data Op1

-------------------------------------------------------------------------------

data Op2

-------------------------------------------------------------------------------
