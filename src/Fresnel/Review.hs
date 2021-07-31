module Fresnel.Review
( -- * Reviews
  Review
  -- * Construction
, unto
) where

import Data.Bifunctor
import Data.Profunctor
import Data.Void
import Fresnel.Optic

-- Reviews

type Review t b = forall p . (Bifunctor p, Profunctor p) => Optic' p t b


-- Construction

unto :: (b -> t) -> Review t b
unto f = first absurd . lmap absurd . rmap f
