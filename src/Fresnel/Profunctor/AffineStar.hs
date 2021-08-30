{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Fresnel.Profunctor.AffineStar
( -- * Affine star profunctors
  AffineStar(..)
  -- * Construction
, affineStar
  -- * Elimination
, runAffineStar
  -- * Computation
, mapAffineStar
) where

import Data.Profunctor
import Data.Functor.Contravariant
import Fresnel.Bifunctor.Contravariant

-- Affine star profunctors

newtype AffineStar f a b = AffineStar { withAffineStar :: forall r . ((forall x . x -> f x) -> (a -> f b) -> r) -> r }

instance Functor f => Profunctor (AffineStar f) where
  dimap f g = mapAffineStar (dimap f (fmap g))
  lmap f = mapAffineStar (lmap f)

instance Functor f => Choice (AffineStar f) where
  left'  (AffineStar r) = AffineStar (\ k -> r (\ point f -> k point (either (fmap Left . f) (fmap Right . point))))
  right' (AffineStar r) = AffineStar (\ k -> r (\ point f -> k point (either (fmap Left . point) (fmap Right . f))))

instance Functor f => Strong (AffineStar f) where
  first'  = mapAffineStar (\ f (a, c) -> (,c) <$> f a)
  second' = mapAffineStar (\ f (c, a) -> (c,) <$> f a)

instance Contravariant f => Bicontravariant (AffineStar f) where
  contrabimap f g = mapAffineStar (\ h -> contramap g . h . f)


-- Construction

affineStar :: (forall x . x -> f x) -> (a -> f b) -> AffineStar f a b
affineStar point f = AffineStar (\ k -> k point f)


-- Elimination

runAffineStar :: AffineStar f a b -> (a -> f b)
runAffineStar a = withAffineStar a (\ _ f -> f)


-- Computation

mapAffineStar :: ((a -> f b) -> (c -> f d)) -> (AffineStar f a b -> AffineStar f c d)
mapAffineStar f (AffineStar r) = AffineStar (\ k -> r (\ point -> k point . f))
