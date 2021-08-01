{-# LANGUAGE RankNTypes #-}
module Fresnel.Review
( -- * Reviews
  Review
  -- * Construction
, unto
, reviewing
  -- * Elimination
, reviews
, review
, (#)
  -- * Utilities
, lphantom
) where

import Data.Bifunctor
import Data.Profunctor
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Void
import Fresnel.Optic
import Fresnel.Profunctor.Recall

-- Reviews

type Review t b = forall p . (Bifunctor p, Choice p) => Optic' p t b


-- Construction

unto :: (b -> t) -> Review t b
unto f = lphantom . rmap f


reviewing :: (Profunctor p, Bifunctor p) => Optic p s t a b -> Optic' p t b
reviewing l f = lphantom . l $ lphantom f


-- Elimination

reviews :: Review t b -> (e -> b) -> (e -> t)
reviews b = runRecall #. b .# Recall

review :: Review t b -> (b -> t)
review b = reviews b id

(#) :: Review t b -> (b -> t)
(#) = review

infixr 8 #


-- Utilities

lphantom :: (Bifunctor p, Profunctor p) => p b c -> p a c
lphantom = first absurd . lmap absurd
