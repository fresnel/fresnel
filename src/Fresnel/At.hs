{-# LANGUAGE RankNTypes #-}
module Fresnel.At
( -- * Updateable collections
  At(..)
, atSet
  -- * Indexable collections
, module Fresnel.Ixed
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

class Ixed c => At c where
  at :: Index c -> Lens' c (Maybe (IxValue c))

instance At IntSet.IntSet where
  at = atSet IntSet.member IntSet.insert

instance At (IntMap.IntMap v) where
  at k = lens (IntMap.lookup k) (\ m -> maybe m (flip (IntMap.insert k) m))

instance Ord k => At (Set.Set k) where
  at = atSet Set.member Set.insert

instance Ord k => At (Map.Map k v) where
  at k = lens (Map.lookup k) (\ m -> maybe m (flip (Map.insert k) m))

instance (Eq k, Hashable k) => At (HashSet.HashSet k) where
  at = atSet HashSet.member HashSet.insert

instance (Eq k, Hashable k) => At (HashMap.HashMap k v) where
  at k = lens (HashMap.lookup k) (\ m -> maybe m (flip (HashMap.insert k) m))


atSet :: (Index c -> c -> Bool) -> (Index c -> c -> c) -> Index c -> Lens' c (Maybe ())
atSet member insert k = lens (guard . member k) (\ s -> maybe s (const (insert k s)))
