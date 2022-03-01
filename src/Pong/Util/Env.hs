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
import Pong.Data
import Pong.Util
import Prelude hiding (insert, lookup, map)
import qualified Data.Map.Strict as Map

{-# INLINE empty #-}
empty :: Environment a
empty = mempty
-- TODO: use Newtype generics

insert :: Name -> a -> Environment a -> Environment a
insert key val (Env envMap) = Env (Map.insert key val envMap)

{-# INLINE inserts #-}
inserts :: [(Name, a)] -> Environment a -> Environment a
inserts = flip (foldr (uncurry insert))

insertWith :: (a -> a -> a) -> Name -> a -> Environment a -> Environment a
insertWith f key val (Env envMap) = Env (Map.insertWith f key val envMap)

{-# INLINE fromList #-}
fromList :: [(Name, a)] -> Environment a
fromList = Env . Map.fromList

{-# INLINE fromListWith #-}
fromListWith :: (a -> a -> a) -> [(Name, a)] -> Environment a
fromListWith f = Env . Map.fromListWith f

{-# INLINE toList #-}
toList :: Environment a -> [(Name, a)]
toList = Map.toList . getEnvironment

union :: Environment a -> Environment a -> Environment a
union = (<>)

elems :: Environment a -> [a]
elems (Env envMap) = Map.elems envMap

domain :: Environment a -> [Name]
domain (Env envMap) = Map.keys envMap

lookup :: Name -> Environment a -> Maybe a
lookup name (Env envMap) = Map.lookup name envMap

findWithDefault :: a -> Name -> Environment a -> a
findWithDefault value key (Env envMap) = Map.findWithDefault value key envMap

findWithDefaultEmpty :: (Monoid a) => Name -> Environment a -> a
findWithDefaultEmpty key (Env envMap) = Map.findWithDefault mempty key envMap

isMember :: Name -> Environment a -> Bool
isMember name (Env envMap) = Map.member name envMap

update :: (a -> Maybe a) -> Name -> Environment a -> Environment a
update f name (Env envMap) = Env (Map.update f name envMap)

alter :: (Maybe a -> Maybe a) -> Name -> Environment a -> Environment a
alter f name (Env envMap) = Env (Map.alter f name envMap)

delete :: Name -> Environment a -> Environment a
delete key (Env envMap) = Env (Map.delete key envMap)

map :: (a -> b) -> Environment a -> Environment b
map f (Env envMap) = Env (Map.map f envMap)

askLookup :: (MonadReader (Environment a) m) => Name -> m (Maybe a)
askLookup = asks . lookup
