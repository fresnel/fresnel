{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Fresnel.At
( -- * Indexable collections
  Ixed(..)
) where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Profunctor.Traversing (Traversing(..))
import           Fresnel.Traversal (Traversal')

-- Indexable collections

class Ixed c where
  type Index c
  type IxValue c

  ix :: Index c -> Traversal' c (IxValue c)

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
