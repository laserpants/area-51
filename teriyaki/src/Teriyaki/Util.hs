module Teriyaki.Util
  ( module Data.Fix
  , module Data.Text
  , module Data.Eq.Deriving
  , module Data.Ord.Deriving
  , module Text.Show.Deriving
  , module Data.Tuple.Extra
  , module Data.Void
  , (<$$>)
  , Name
  , Algebra
  , Coalgebra
  , Data
  , Typeable
  , project
  , embed
  , embed1
  , embed2
  , embed3
  , embed4
  , embed5
  )
where

import Data.Data (Data)
import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (Base, Corecursive, embed, project)
import Data.Ord.Deriving (deriveOrd1)
import Data.Text (Text)
import Data.Tuple.Extra (first, second)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Text.Show.Deriving (deriveShow1)

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = ((f <$>) <$>)

infixl 4 <$$>

type Name = Text

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

{-# INLINE embed1 #-}
embed1 :: (Corecursive t) => (t1 -> Base t t) -> t1 -> t
embed1 t a = embed (t a)

{-# INLINE embed2 #-}
embed2 :: (Corecursive t) => (t1 -> t2 -> Base t t) -> t1 -> t2 -> t
embed2 t a b = embed (t a b)

{-# INLINE embed3 #-}
embed3 :: (Corecursive t) => (t1 -> t2 -> t3 -> Base t t) -> t1 -> t2 -> t3 -> t
embed3 t a b c = embed (t a b c)

{-# INLINE embed4 #-}
embed4 ::
  (Corecursive t) =>
  (t1 -> t2 -> t3 -> t4 -> Base t t) ->
  t1 ->
  t2 ->
  t3 ->
  t4 ->
  t
embed4 t a b c d = embed (t a b c d)

{-# INLINE embed5 #-}
embed5 ::
  (Corecursive t) =>
  (t1 -> t2 -> t3 -> t4 -> t5 -> Base t t) ->
  t1 ->
  t2 ->
  t3 ->
  t4 ->
  t5 ->
  t
embed5 t a b c d e = embed (t a b c d e)
