module Fresnel.Setter
( -- * Setters
  Setter
) where

import Data.Profunctor.Mapping
import Fresnel.Optic

-- Setters

type Setter s t a b = forall p . Mapping p => Optic p s t a b
