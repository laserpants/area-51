{-# LANGUAGE StrictData #-}

module Teriyaki.Data where

import Teriyaki.Util

data KindF a
  = KCon Name
  | KArr a a

type Kind = Fix KindF

data TypeF a
  = TUnit

data Prim
  = PUnit

type Type = Fix TypeF

data PatternF a
  = PVar

data ExprF a
  = EVar
