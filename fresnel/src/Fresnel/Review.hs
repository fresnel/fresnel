{-# LANGUAGE RankNTypes #-}
module Fresnel.Review
( -- * Reviews
  Review
, IsReview
  -- * Construction
, unto
, reviewing
, un
  -- * Elimination
, reviews
, review
, (#)
, re
  -- * Utilities
, lphantom
) where

import Data.Bifunctor
import Data.Profunctor
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Void
import Fresnel.Getter (Getter, to, view)
import Fresnel.Optic
import Fresnel.Prism.Internal (IsPrism)
import Fresnel.Profunctor.Recall

-- Reviews

type Review t b = forall p . IsReview p => Optic' p t b

class (IsPrism p, Bifunctor p, Costrong p) => IsReview p

instance IsReview (Recall e)


-- Construction

unto :: (b -> t) -> Review t b
unto f = lphantom . rmap f


reviewing :: (Profunctor p, Bifunctor p) => Optic p s t a b -> Optic' p t b
reviewing l f = lphantom . l $ lphantom f


un :: Getter s a -> Review a s
un o = unto (view o)


-- Elimination

reviews :: Review t b -> (e -> b) -> (e -> t)
reviews b = runRecall #. b .# Recall

review :: Review t b -> (b -> t)
review b = reviews b id

(#) :: Review t b -> (b -> t)
(#) = review

infixr 8 #


re :: Review t b -> Getter b t
re o = to (review o)


-- Utilities

lphantom :: (Bifunctor p, Profunctor p) => p b c -> p a c
lphantom = first absurd . lmap absurd
