module Fresnel.Getter
( -- * Getters
  Getter
) where

import Data.Profunctor
import Fresnel.Optic

-- Getters

type Getter s a = forall p . (Strong p, Cochoice p) => Optic' p s a
