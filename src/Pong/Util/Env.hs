{-# LANGUAGE FlexibleContexts #-}
module Pong.Util.Env
  ( empty
  , insert
  , inserts
  , insertWith
  , fromList
  , fromListWith
  , toList
  , union
  , elems
  , domain
  , lookup
  , findWithDefault
  , findWithDefaultEmpty
  , isMember
  , update
  , alter
  , delete
  , map
  , askLookup
  ) where

import Control.Monad.Reader
import Control.Newtype.Generics
import Pong.Data
import Pong.Util (Name, (<$$>))
import Prelude hiding (insert, lookup, map)
import qualified Data.Map.Strict as Map

{-# INLINE empty #-}
empty :: Environment a
empty = mempty

{-# INLINE insert #-}
insert :: Name -> a -> Environment a -> Environment a
insert = over Env <$$> Map.insert 

{-# INLINE inserts #-}
inserts :: [(Name, a)] -> Environment a -> Environment a
inserts = flip (foldr (uncurry insert))

insertWith :: (a -> a -> a) -> Name -> a -> Environment a -> Environment a
insertWith f key val = over Env (Map.insertWith f key val)

{-# INLINE fromList #-}
fromList :: [(Name, a)] -> Environment a
fromList = pack . Map.fromList

{-# INLINE fromListWith #-}
fromListWith :: (a -> a -> a) -> [(Name, a)] -> Environment a
fromListWith = pack <$$> Map.fromListWith 

{-# INLINE toList #-}
toList :: Environment a -> [(Name, a)]
toList = Map.toList . unpack

{-# INLINE union #-}
union :: Environment a -> Environment a -> Environment a
union = (<>)

{-# INLINE elems #-}
elems :: Environment a -> [a]
elems = Map.elems . unpack

{-# INLINE domain #-}
domain :: Environment a -> [Name]
domain = Map.keys . unpack

{-# INLINE lookup #-}
lookup :: Name -> Environment a -> Maybe a
lookup name = Map.lookup name . unpack

{-# INLINE findWithDefault #-}
findWithDefault :: a -> Name -> Environment a -> a
findWithDefault value key = Map.findWithDefault value key . unpack

{-# INLINE findWithDefaultEmpty #-}
findWithDefaultEmpty :: (Monoid a) => Name -> Environment a -> a
findWithDefaultEmpty key = Map.findWithDefault mempty key . unpack

{-# INLINE isMember #-}
isMember :: Name -> Environment a -> Bool
isMember name = Map.member name . unpack

{-# INLINE update #-}
update :: (a -> Maybe a) -> Name -> Environment a -> Environment a
update = over Env <$$> Map.update 

{-# INLINE alter #-}
alter :: (Maybe a -> Maybe a) -> Name -> Environment a -> Environment a
alter = over Env <$$> Map.alter

{-# INLINE delete #-}
delete :: Name -> Environment a -> Environment a
delete = over Env . Map.delete

{-# INLINE map #-}
map :: (a -> b) -> Environment a -> Environment b
map = over Env . Map.map 

{-# INLINE askLookup #-}
askLookup :: (MonadReader (Environment a) m) => Name -> m (Maybe a)
askLookup = asks . lookup
