{-# LANGUAGE RankNTypes #-}
module Fresnel.Set
( setmapped
, setOf
) where

import Data.Set as Set
import Fresnel.Fold
import Fresnel.Setter

setmapped :: Ord b => Setter (Set a) (Set b) a b
setmapped = sets Set.map

setOf :: Ord a => Fold s a -> (s -> Set.Set a)
setOf o = Set.fromList . toListOf o
