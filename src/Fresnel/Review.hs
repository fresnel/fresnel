module Fresnel.Review
( -- * Reviews
  Review
  -- * Construction
, unto
  -- * Elimination
, reviews
, review
, (#)
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
unto f = first absurd . lmap absurd . rmap f


-- Elimination

reviews :: Optic (Recall e) s t a b -> (e -> b) -> (e -> t)
reviews b = runRecall . b . Recall

review :: Optic (Recall b) s t a b -> (b -> t)
review b = reviews b id

(#) :: Optic (Recall b) s t a b -> (b -> t)
(#) = review

infixr 8 #
