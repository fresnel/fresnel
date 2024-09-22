{-# LANGUAGE RankNTypes #-}
module Fresnel.Profunctor.Traversing1
( Traversing1(..)
  -- ** Profunctor from Traversing1
, dimapTraversing1
, lmapTraversing1
, rmapTraversing1
  -- ** Strong from Traversing1
, firstTraversing1
, secondTraversing1
) where

import Control.Arrow (Kleisli(..))
import Data.Functor.Apply
import Data.Functor.Const
import Data.Profunctor (Forget(..), Star(..), Strong)
import Data.Profunctor.Unsafe ((#.))
import Fresnel.Profunctor.OptionalStar (OptionalStar(..))

class Strong p => Traversing1 p where
  wander1 :: (forall f . Apply f => (a -> f b) -> (s -> f t)) -> (p a b -> p s t)

instance Monad m => Traversing1 (Kleisli m) where
  wander1 f (Kleisli k) = Kleisli (unwrapApplicative . f (WrapApplicative . k))

instance Semigroup r => Traversing1 (Forget r) where
  wander1 f (Forget k) = Forget (getConst #. f (Const #. k))

instance Apply f => Traversing1 (Star f) where
  wander1 f (Star k) = Star (f k)

instance Apply f => Traversing1 (OptionalStar f) where
  wander1 f (OptionalStar k) = OptionalStar (\ k' -> k (\ p -> k' p . f))


-- Profunctor from Traversing1

dimapTraversing1 :: Traversing1 p => (a' -> a) -> (b -> b') -> (p a b -> p a' b')
dimapTraversing1 f g = wander1 (\ k -> fmap g . k . f)

lmapTraversing1 :: Traversing1 p => (a' -> a) -> (p a b -> p a' b)
lmapTraversing1 f = wander1 (. f)

rmapTraversing1 :: Traversing1 p => (b -> b') -> (p a b -> p a b')
rmapTraversing1 f = wander1 (fmap f .)


-- Strong from Traversing1


firstTraversing1 :: Traversing1 p => p a b -> p (a, c) (b, c)
firstTraversing1 = wander1 (\ k (a, c) -> flip (,) c <$> k a)

secondTraversing1 :: Traversing1 p => p a b -> p (c, a) (c, b)
secondTraversing1 = wander1 (\ k (c, a) -> (,) c <$> k a)
