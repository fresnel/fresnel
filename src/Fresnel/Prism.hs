module Fresnel.Prism
( -- * Prisms
  Prism
) where

import Data.Profunctor
import Fresnel.Optic

-- Prisms

type Prism s t a b = forall p . Choice p => Optic p s t a b
