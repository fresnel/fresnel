{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Fresnel.Ixed
( -- * Indexable collections
  Ixed(..)
) where

import qualified Data.HashMap.Internal as HashMap
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import           Data.Profunctor.Traversing (Traversing(..))
import qualified Data.Set as Set
import           Fresnel.Traversal (Traversal')

-- Indexable collections

class Ixed c where
  type Index c
  type IxValue c

  ix :: Index c -> Traversal' c (IxValue c)

instance Ixed IntSet.IntSet where
  type Index IntSet.IntSet = IntSet.Key
  type IxValue IntSet.IntSet = ()

  ix k = wander $ \ f s -> if IntSet.member k s
    then IntSet.insert k s <$ f ()
    else pure s

instance Ixed (IntMap.IntMap v) where
  type Index (IntMap.IntMap v) = IntMap.Key
  type IxValue (IntMap.IntMap v) = v

  ix k = wander $ \ f m -> case IntMap.lookup k m of
    Just v  -> flip (IntMap.insert k) m <$> f v
    Nothing -> pure m

instance Ord k => Ixed (Map.Map k v) where
  type Index (Map.Map k v) = k
  type IxValue (Map.Map k v) = v

  ix k = wander $ \ f m -> case Map.lookup k m of
    Just v  -> flip (Map.insert k) m <$> f v
    Nothing -> pure m

instance Ord k => Ixed (Set.Set k) where
  type Index (Set.Set k) = k
  type IxValue (Set.Set k) = ()

  ix k = wander $ \ f s -> if Set.member k s
    then Set.insert k s <$ f ()
    else pure s

instance (Eq k, Hashable k) => Ixed (HashMap.HashMap k v) where
  type Index (HashMap.HashMap k v) = k
  type IxValue (HashMap.HashMap k v) = v

  ix k = wander $ \ f m -> case HashMap.lookup k m of
    Just v  -> flip (HashMap.insert k) m <$> f v
    Nothing -> pure m

instance (Eq k, Hashable k) => Ixed (HashSet.HashSet k) where
  type Index (HashSet.HashSet k) = k
  type IxValue (HashSet.HashSet k) = ()

  ix k = wander $ \ f s -> if HashSet.member k s
    then HashSet.insert k s <$ f ()
    else pure s
