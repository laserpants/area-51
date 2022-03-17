{-# LANGUAGE FlexibleContexts #-}

module Pong.Util
  ( module Control.Arrow
  , module Data.Functor.Foldable
  , module Data.Fix
  , module Data.Map.Strict
  , module Data.Text
  , (<$$>)
  , (<#>)
  , Name
  , Names
  , Algebra
  , Coalgebra
  , List1
  , embed1
  , embed2
  , embed3
  , embed4
  , embed5
  , without
  , localFirst
  , asksFirst
  , localSecond
  , asksSecond
  , trimLabel
  ) where

import Control.Arrow ((***), (<<<), (>>>))
import Control.Monad.Reader
import Data.Fix (Fix(..))
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map, (!), (!?))
import Data.Text (Text, pack, unpack)
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

{-# INLINE without #-}
without :: (Eq a) => [a] -> [a] -> [a]
without = foldr (filter <<< (/=))

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = ((f <$>) <$>)

infixl 4 <$$>

{-# INLINE (<#>) #-}
(<#>) :: (Functor f) => f a -> (a -> b) -> f b
(<#>) = flip (<$>)

infixl 1 <#>

type Name = Text

type Names = [Name]

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

type List1 = NonEmpty

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
     (Corecursive t)
  => (t1 -> t2 -> t3 -> t4 -> Base t t)
  -> t1
  -> t2
  -> t3
  -> t4
  -> t
embed4 t a b c d = embed (t a b c d)

{-# INLINE embed5 #-}
embed5 ::
     (Corecursive t)
  => (t1 -> t2 -> t3 -> t4 -> t5 -> Base t t)
  -> t1
  -> t2
  -> t3
  -> t4
  -> t5
  -> t
embed5 t a b c d e = embed (t a b c d e)

{-# INLINE localFirst #-}
localFirst :: MonadReader (q, r) m => (q -> q) -> m a -> m a
localFirst = local . first

{-# INLINE asksFirst #-}
asksFirst :: MonadReader (q, r) m => (q -> a) -> m a
asksFirst = asks . (fst >>>)

{-# INLINE localSecond #-}
localSecond :: MonadReader (q, r) m => (r -> r) -> m a -> m a
localSecond = local . second

{-# INLINE asksSecond #-}
asksSecond :: MonadReader (q, r) m => (r -> a) -> m a
asksSecond = asks . (snd >>>)

{-# INLINE trimLabel #-}
trimLabel :: Name -> Name
trimLabel = Text.tail . Text.init
