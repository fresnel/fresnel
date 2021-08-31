{-# LANGUAGE RankNTypes #-}
module Fresnel.AffineFold
( -- * Affine folds
  AffineFold
, IsAffineFold
  -- * Construction
, afolding
, filtered
  -- * Elimination
, previews
, preview
, (^?)
) where

import Data.Monoid (First(..))
import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Optic
import Fresnel.Profunctor.Optical

-- Affine folds

type AffineFold s a = forall p . IsAffineFold p => Optic' p s a


-- Construction

afolding :: (s -> Maybe a) -> AffineFold s a
afolding f = contrabimap ((`maybe` Right) . Left <*> f) Left . right'

filtered :: (a -> Bool) -> AffineFold a a
filtered p = afolding (\ a -> if p a then Just a else Nothing)


-- Elimination

previews :: AffineFold s a -> (a -> r) -> (s -> Maybe r)
previews o f = getFirst . runForget (o (Forget (First . Just . f)))

preview :: AffineFold s a -> s -> Maybe a
preview o = previews o id

(^?) :: s -> AffineFold s a -> Maybe a
s ^? o = preview o s

infixl 8 ^?
