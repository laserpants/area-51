{-# LANGUAGE FlexibleContexts #-}

module Pong.Util
  ( module Control.Arrow
  , module Data.Functor.Foldable
  , module Data.Fix
  , module Data.Void
  , module Data.Eq.Deriving
  , module Data.Functor
  , module Data.Ord.Deriving
  , module Text.Show.Deriving
  , module Data.Map.Strict
  , module Data.Text
  , (<$$>)
  , Name
  , Algebra
  , Coalgebra
  , List1
  , embed1
  , embed2
  , embed3
  , embed4
  , embed5
  , without
  , withoutLabels
  , localFirst
  , asksFirst
  , localSecond
  , asksSecond
  , getAndModify
  , mapFoldrWithKeyM
  , varSequence
  )
where

import Control.Arrow ((***), (<<<), (>>>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import Data.Ord.Deriving (deriveOrd1)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Tuple.Extra (first, second)
import Data.Void (Void)
import Text.Show.Deriving (deriveShow1)
import TextShow (showt)

{-# INLINE without #-}
without :: (Eq a) => [a] -> [a] -> [a]
without = foldr (filter <<< (/=))

withoutLabels :: [Name] -> [(t, Name)] -> [(t, Name)]
withoutLabels elems = filter (\a -> snd a `notElem` Set.fromList elems)

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = ((f <$>) <$>)

infixl 4 <$$>

type Name = Text

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

getAndModify :: (MonadState s m) => (s -> s) -> m s
getAndModify f = do
  s <- get
  modify f
  pure s

mapFoldrWithKeyM :: (Monad m) => ((k, a) -> b -> m b) -> b -> Map k a -> m b
mapFoldrWithKeyM f a = foldrM f a . Map.toList

varSequence :: Text -> [a] -> [(a, Text)]
varSequence prefix names = names `zip` [prefix <> showt i | i <- [0 :: Int ..]]
