{-# LANGUAGE RankNTypes #-}
module Fresnel.Profunctor.Traversing1
( Traversing1(..)
  -- ** Profunctor from Traversing1
, dimapTraversing1
, lmapTraversing1
  -- ** Strong from Traversing1
, firstTraversing1
, secondTraversing1
) where

import Data.Functor.Apply
import Data.Profunctor.Strong

class Strong p => Traversing1 p where
  wander1 :: (forall f . Apply f => (a -> f b) -> (s -> f t)) -> (p a b -> p s t)


-- Profunctor from Traversing1

dimapTraversing1 :: Traversing1 p => (a' -> a) -> (b -> b') -> (p a b -> p a' b')
dimapTraversing1 f g = wander1 (\ k -> fmap g . k . f)

lmapTraversing1 :: Traversing1 p => (a' -> a) -> (p a b -> p a' b)
lmapTraversing1 f = wander1 (. f)


-- Strong from Traversing1


firstTraversing1 :: Traversing1 p => p a b -> p (a, c) (b, c)
firstTraversing1 = wander1 (\ k (a, c) -> flip (,) c <$> k a)

secondTraversing1 :: Traversing1 p => p a b -> p (c, a) (c, b)
secondTraversing1 = wander1 (\ k (c, a) -> (,) c <$> k a)
