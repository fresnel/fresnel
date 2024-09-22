{-# LANGUAGE RankNTypes #-}
module Fresnel.Profunctor.Traversing1
( Traversing1(..)
) where

import Data.Functor.Apply
import Data.Profunctor.Strong

class Strong p => Traversing1 p where
  wander1 :: (forall f . Apply f => (a -> f b) -> (s -> f t)) -> (p a b -> p s t)
