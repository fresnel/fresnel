{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Fresnel.Profunctor.OptionalStar
( -- * Optional star profunctors
  OptionalStar(..)
  -- * Construction
, optionalStar
  -- * Elimination
, runOptionalStar
  -- * Computation
, mapOptionalStar
) where

import Data.Coerce
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Profunctor.Unsafe
import Fresnel.Bifunctor.Contravariant

-- Optional star profunctors

newtype OptionalStar f a b = OptionalStar { withOptionalStar :: forall r . ((forall x . x -> f x) -> (a -> f b) -> r) -> r }

instance Functor f => Profunctor (OptionalStar f) where
  dimap f g = mapOptionalStar (dimap f (fmap g))
  lmap f = mapOptionalStar (lmap f)
  rmap g = mapOptionalStar (rmap (fmap g))
  (.#) = fmap coerce . const

instance Functor f => Choice (OptionalStar f) where
  left'  (OptionalStar r) = OptionalStar (\ k -> r (\ point f -> k point (either (fmap Left . f) (fmap Right . point))))
  right' (OptionalStar r) = OptionalStar (\ k -> r (\ point f -> k point (either (fmap Left . point) (fmap Right . f))))

instance Traversable f => Cochoice (OptionalStar f) where
  unright r = withOptionalStar r $ \ point f -> let go = either (go . Left) id . sequenceA . f in optionalStar point (go . Right)

instance Functor f => Strong (OptionalStar f) where
  first'  = mapOptionalStar (\ f (a, c) -> (,c) <$> f a)
  second' = mapOptionalStar (\ f (c, a) -> (c,) <$> f a)

instance Contravariant f => Bicontravariant (OptionalStar f) where
  contrabimap f g = mapOptionalStar (\ h -> contramap g . h . f)


-- Construction

optionalStar :: (forall x . x -> f x) -> (a -> f b) -> OptionalStar f a b
optionalStar point f = OptionalStar (\ k -> k point f)


-- Elimination

runOptionalStar :: OptionalStar f a b -> (a -> f b)
runOptionalStar a = withOptionalStar a (\ _ f -> f)


-- Computation

mapOptionalStar :: ((a -> f b) -> (c -> f d)) -> (OptionalStar f a b -> OptionalStar f c d)
mapOptionalStar f (OptionalStar r) = OptionalStar (\ k -> r (\ point -> k point . f))
