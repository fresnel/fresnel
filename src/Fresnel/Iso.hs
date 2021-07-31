module Fresnel.Iso
( -- * Isos
  Iso
, Iso'
) where

import Data.Profunctor
import Fresnel.Optic

-- Isos

type Iso s t a b = forall p . Profunctor p => Optic p s t a b

type Iso' s a = Iso s s a a
