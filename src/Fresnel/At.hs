{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Fresnel.At
( -- * Indexable collections
  Ixed(..)
) where

import           Data.Functor ((<&>))
import qualified Data.IntMap as IntMap
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
    Just v  -> f v <&> \ v' -> IntMap.insert k v' m
    Nothing -> pure m
