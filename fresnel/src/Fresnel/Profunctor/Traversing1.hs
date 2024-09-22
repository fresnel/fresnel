{-# LANGUAGE RankNTypes #-}
module Fresnel.Profunctor.Traversing1
( Traversing1(..)
  -- ** Profunctor from Traversing1
, dimapTraversing1
) where

import Data.Functor.Apply
import Data.Profunctor.Strong

class Strong p => Traversing1 p where
  wander1 :: (forall f . Apply f => (a -> f b) -> (s -> f t)) -> (p a b -> p s t)


-- Profunctor from Traversing1

dimapTraversing1 :: Traversing1 p => (a' -> a) -> (b -> b') -> (p a b -> p a' b')
dimapTraversing1 f g = wander1 (\ k -> fmap g . k . f)
