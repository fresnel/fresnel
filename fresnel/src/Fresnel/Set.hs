{-# LANGUAGE RankNTypes #-}
module Fresnel.Set
( setOf
) where

import Data.Set as Set
import Fresnel.Fold

setOf :: Ord a => Fold s a -> (s -> Set.Set a)
setOf o = Set.fromList . toListOf o
