{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}

module Taiyaki.Util
  ( module Data.Fix
  , module Data.Text
  , module Control.Arrow
  , module Data.Map.Strict
  , module Data.Set.Monad
  , module Data.Eq.Deriving
  , module Data.Functor.Foldable
  , module Data.Ord.Deriving
  , module Text.Show.Deriving
  , module Data.Tuple.Extra
  , module Data.List.Extra
  , module Data.Void
  , module Data.Function
  , module TextShow
  , (<$$>)
  , Name
  , Algebra
  , Coalgebra
  , Data
  , Typeable
  , Void1
  , embed1
  , embed2
  , embed3
  , embed4
  , embed5
  , foldr2
  , getAndModify
  )
where

import Control.Arrow ((<<<), (>>>))
import Control.Monad.State
import Data.Data (Data)
import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Function ((&))
import Data.Functor.Foldable (Base, Corecursive, cata, embed, para, project)
import Data.List.Extra (singleton)
import Data.Map.Strict (Map)
import Data.Ord.Deriving (deriveOrd1)
import Data.Set.Monad (Set)
import Data.Text (Text)
import Data.Tuple.Extra (both, first, fst3, second, snd3, swap, thd3)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Text.Show.Deriving (deriveShow1)
import TextShow (showt)

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = ((f <$>) <$>)

infixl 4 <$$>

type Name = Text

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

data Void1 a

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

{-# INLINE foldr2 #-}
foldr2 :: (Foldable f, Foldable g) => (a -> b -> b) -> b -> f (g a) -> b
foldr2 = foldr . flip . foldr

getAndModify :: (MonadState s m) => (s -> s) -> m s
getAndModify f = do
  s <- get
  modify f
  pure s

deriving instance Show (Void1 a)

deriving instance Eq (Void1 a)

deriving instance Ord (Void1 a)

deriving instance Functor Void1

deriveShow1 ''Void1

deriveEq1 ''Void1

deriveOrd1 ''Void1
