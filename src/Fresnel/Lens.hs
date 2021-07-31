module Fresnel.Lens
( -- * Lenses
  Lens
) where

import Data.Profunctor
import Fresnel.Optic

-- Lenses

type Lens s t a b = forall p . Strong p => Optic p s t a b
