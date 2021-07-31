module Fresnel.Setter
( -- * Setters
  Setter
, Setter'
  -- * Construction
, sets
) where

import Data.Profunctor.Mapping
import Fresnel.Optic

-- Setters

type Setter s t a b = forall p . Mapping p => Optic p s t a b

type Setter' s a = Setter s s a a


-- Construction

sets :: ((a -> b) -> (s -> t)) -> Setter s t a b
sets = roam
