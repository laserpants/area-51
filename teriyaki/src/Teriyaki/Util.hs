module Teriyaki.Util
  ( module Data.Fix
  , module Data.Text
  , module Data.Eq.Deriving
  , module Data.Ord.Deriving
  , module Text.Show.Deriving
  , (<$$>)
  , Name
  , Algebra
  , Coalgebra
  )
where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Ord.Deriving (deriveOrd1)
import Data.Text
import Text.Show.Deriving (deriveShow1)

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = ((f <$>) <$>)

infixl 4 <$$>

type Name = Text

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a
