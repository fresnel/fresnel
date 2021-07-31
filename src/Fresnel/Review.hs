module Fresnel.Review
( -- * Reviews
  Review
  -- * Construction
, unto
  -- * Elimination
, reviews
, review
, (#)
  -- * Utilities
, lphantom
) where

import Data.Bifunctor
import Data.Profunctor
import Data.Void
import Fresnel.Optic
import Fresnel.Profunctor.Recall

-- Reviews

type Review t b = forall p . (Bifunctor p, Profunctor p) => Optic' p t b


-- Construction

unto :: (b -> t) -> Review t b
unto f = lphantom . rmap f


-- Elimination

reviews :: Optic (Recall e) s t a b -> (e -> b) -> (e -> t)
reviews b = runRecall . b . Recall

review :: Optic (Recall b) s t a b -> (b -> t)
review b = reviews b id

(#) :: Optic (Recall b) s t a b -> (b -> t)
(#) = review

infixr 8 #


-- Utilities

lphantom :: (Bifunctor p, Profunctor p) => p b c -> p a c
lphantom = first absurd . lmap absurd
