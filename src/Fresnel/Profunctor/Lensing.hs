module Fresnel.Profunctor.Lensing
( -- * Lens constraints
  Lensing
) where

import Data.Profunctor
import Fresnel.Profunctor.Isoing

-- Lens constraints

class (Isoing p, Strong p) => Lensing p
