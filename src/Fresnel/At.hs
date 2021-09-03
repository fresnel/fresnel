{-# LANGUAGE RankNTypes #-}
module Fresnel.At
( -- * Updateable collections
  At(..)
, atSet
, atMap
, atList
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
  at = atMap IntMap.lookup IntMap.insert

instance Ord k => At (Set.Set k) where
  at = atSet Set.member Set.insert

instance Ord k => At (Map.Map k v) where
  at = atMap Map.lookup Map.insert

instance (Eq k, Hashable k) => At (HashSet.HashSet k) where
  at = atSet HashSet.member HashSet.insert

instance (Eq k, Hashable k) => At (HashMap.HashMap k v) where
  at = atMap HashMap.lookup HashMap.insert


atSet :: (Index c -> c -> Bool) -> (Index c -> c -> c) -> Index c -> Lens' c (Maybe ())
atSet member insert k = lens (guard . member k) (\ s -> maybe s (const (insert k s)))

atMap :: (Index c -> c -> Maybe (IxValue c)) -> (Index c -> IxValue c -> c -> c) -> Index c -> Lens' c (Maybe (IxValue c))
atMap lookup insert k = lens (lookup k) (\ m -> maybe m (flip (insert k) m))

atList :: Int -> Lens' [a] (Maybe a)
atList i = lens (get i) (\ as -> maybe as (set i as))
  where
  get i as = case as of
    []   -> Nothing
    a:as -> if i <= 0 then Just a else get (i - 1) as
  set i as a' = case as of
    []   -> as
    a:as -> if i <= 0 then a':as else a : set (i - 1) as a'
