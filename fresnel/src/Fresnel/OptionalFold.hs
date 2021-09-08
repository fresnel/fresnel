{-# LANGUAGE RankNTypes #-}
module Fresnel.OptionalFold
( -- * Optional folds
  OptionalFold
, IsOptionalFold
  -- * Construction
, afolding
, filtered
  -- * Elimination
, previews
, preview
, (^?)
, isn't
) where

import Data.Maybe (isJust)
import Data.Monoid (First(..))
import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Optic
import Fresnel.Profunctor.Optical

-- Optional folds

type OptionalFold s a = forall p . IsOptionalFold p => Optic' p s a


-- Construction

afolding :: (s -> Maybe a) -> OptionalFold s a
afolding f = contrabimap ((`maybe` Right) . Left <*> f) Left . right'

filtered :: (a -> Bool) -> OptionalFold a a
filtered p = afolding (\ a -> if p a then Just a else Nothing)


-- Elimination

previews :: OptionalFold s a -> (a -> r) -> (s -> Maybe r)
previews o f = getFirst . runForget (o (Forget (First . Just . f)))

preview :: OptionalFold s a -> s -> Maybe a
preview o = previews o id

(^?) :: s -> OptionalFold s a -> Maybe a
s ^? o = preview o s

infixl 8 ^?

isn't :: OptionalFold s a -> s -> Bool
isn't o = isJust . preview o
