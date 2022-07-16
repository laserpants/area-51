module Teriyaki.Util
  ( module Data.Fix
  , (<$$>)
  , Name
  , Algebra
  , Coalgebra
  )
where

import Data.Fix (Fix (..))
import Data.Text

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = ((f <$>) <$>)

infixl 4 <$$>

type Name = Text

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a
