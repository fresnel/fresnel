module Fresnel.Review
( -- * Reviews
  Review
) where

import Data.Bifunctor
import Data.Profunctor
import Fresnel.Optic

-- Reviews

type Review t b = forall p . (Bifunctor p, Profunctor p) => Optic' p t b
