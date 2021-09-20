{-# LANGUAGE RankNTypes #-}
module Fresnel.At
( -- * Updateable collections
  At(..)
  -- * Construction
, atSet
, atMap
, ixAt
  -- * Elimination
, sans
) where

import           Control.Monad (guard)
import qualified Data.HashMap.Internal.Strict as HashMap
import qualified Data.HashSet as HashSet
import           Data.Hashable
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Fresnel.Ixed
import           Fresnel.Lens (Lens', lens)
import           Fresnel.Maybe (_Just)
import           Fresnel.Optional (Optional')
import           Fresnel.Setter

-- Updateable collections

class Ixed c => At c where
  at :: Index c -> Lens' c (Maybe (IxValue c))

instance At IntSet.IntSet where
  at = atSet IntSet.member IntSet.insert IntSet.delete

instance At (IntMap.IntMap v) where
  at = atMap IntMap.lookup IntMap.insert IntMap.delete

instance Ord k => At (Set.Set k) where
  at = atSet Set.member Set.insert Set.delete

instance Ord k => At (Map.Map k v) where
  at = atMap Map.lookup Map.insert Map.delete

instance (Eq k, Hashable k) => At (HashSet.HashSet k) where
  at = atSet HashSet.member HashSet.insert HashSet.delete

instance (Eq k, Hashable k) => At (HashMap.HashMap k v) where
  at = atMap HashMap.lookup HashMap.insert HashMap.delete


-- Construction

atSet :: (Index c -> c -> Bool) -> (Index c -> c -> c) -> (Index c -> c -> c) -> Index c -> Lens' c (Maybe ())
atSet member insert delete k = lens (guard . member k) (\ s -> maybe (delete k s) (const (insert k s)))

atMap :: (Index c -> c -> Maybe (IxValue c)) -> (Index c -> IxValue c -> c -> c) -> (Index c -> c -> c) -> Index c -> Lens' c (Maybe (IxValue c))
atMap lookup insert delete k = lens (lookup k) (\ m -> maybe (delete k m) (flip (insert k) m))

ixAt :: At a => Index a -> Optional' a (IxValue a)
ixAt i = at i . _Just


-- Elimination

sans :: At c => Index c -> c -> c
sans k = at k .~ Nothing
