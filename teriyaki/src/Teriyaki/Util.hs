module Teriyaki.Util where

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = ((f <$>) <$>)

infixl 4 <$$>

type Name = Text

type Names = [Name]

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a
