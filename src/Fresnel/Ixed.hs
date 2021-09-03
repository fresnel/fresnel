{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Fresnel.Ixed
( -- * Indexable collections
  Ixed(..)
) where

import           Control.Monad (guard)
import qualified Data.HashMap.Internal as HashMap
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Fresnel.Optional (Optional', optional')

-- Indexable collections

class Ixed c where
  type Index c
  type IxValue c

  ix :: Index c -> Optional' c (IxValue c)

instance Ixed IntSet.IntSet where
  type Index IntSet.IntSet = IntSet.Key
  type IxValue IntSet.IntSet = ()

  ix k = optional' (guard . IntSet.member k) (const . IntSet.insert k)

instance Ixed (IntMap.IntMap v) where
  type Index (IntMap.IntMap v) = IntMap.Key
  type IxValue (IntMap.IntMap v) = v

  ix k = optional' (IntMap.lookup k) (flip (IntMap.insert k))

instance Ord k => Ixed (Map.Map k v) where
  type Index (Map.Map k v) = k
  type IxValue (Map.Map k v) = v

  ix k = optional' (Map.lookup k) (flip (Map.insert k))

instance Ord k => Ixed (Set.Set k) where
  type Index (Set.Set k) = k
  type IxValue (Set.Set k) = ()

  ix k = optional' (guard . Set.member k) (const . Set.insert k)

instance (Eq k, Hashable k) => Ixed (HashMap.HashMap k v) where
  type Index (HashMap.HashMap k v) = k
  type IxValue (HashMap.HashMap k v) = v

  ix k = optional' (HashMap.lookup k) (flip (HashMap.insert k))

instance (Eq k, Hashable k) => Ixed (HashSet.HashSet k) where
  type Index (HashSet.HashSet k) = k
  type IxValue (HashSet.HashSet k) = ()

  ix k = optional' (guard . HashSet.member k) (const . HashSet.insert k)
