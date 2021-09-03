{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Fresnel.Ixed
( -- * Indexable collections
  Ixed(..)
, ixList
) where

import           Control.Monad (guard)
import qualified Data.HashMap.Internal as HashMap
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Fresnel.List.NonEmpty (head_, tail_)
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

instance Ord k => Ixed (Set.Set k) where
  type Index (Set.Set k) = k
  type IxValue (Set.Set k) = ()

  ix k = optional' (guard . Set.member k) (const . Set.insert k)

instance Ord k => Ixed (Map.Map k v) where
  type Index (Map.Map k v) = k
  type IxValue (Map.Map k v) = v

  ix k = optional' (Map.lookup k) (flip (Map.insert k))

instance (Eq k, Hashable k) => Ixed (HashSet.HashSet k) where
  type Index (HashSet.HashSet k) = k
  type IxValue (HashSet.HashSet k) = ()

  ix k = optional' (guard . HashSet.member k) (const . HashSet.insert k)

instance (Eq k, Hashable k) => Ixed (HashMap.HashMap k v) where
  type Index (HashMap.HashMap k v) = k
  type IxValue (HashMap.HashMap k v) = v

  ix k = optional' (HashMap.lookup k) (flip (HashMap.insert k))

instance Ixed [v] where
  type Index [v] = Int
  type IxValue [v] = v

  ix k = ixList k

instance Ixed (NonEmpty.NonEmpty v) where
  type Index (NonEmpty.NonEmpty v) = Int
  type IxValue (NonEmpty.NonEmpty v) = v

  ix k
    | k <= 0    = head_
    | otherwise = tail_.ixList (k - 1)

ixList :: Int -> Optional' [a] a
ixList i = optional' (get i) (set i)
  where
  get i as = case as of
    []   -> Nothing
    a:as -> if i <= 0 then Just a else get (i - 1) as
  set i as a' = case as of
    []   -> as
    a:as -> if i <= 0 then a':as else a : set (i - 1) as a'
