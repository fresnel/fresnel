module Fresnel.Iso
( -- * Isos
  Iso
) where

import Data.Profunctor
import Fresnel.Optic

-- Isos

type Iso s t a b = forall p . Profunctor p => Optic p s t a b
