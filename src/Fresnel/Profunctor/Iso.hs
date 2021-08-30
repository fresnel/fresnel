module Fresnel.Profunctor.Iso
( -- * Iso constraints
  Isoing
) where


import Data.Profunctor

-- Iso constraints

class Profunctor p => Isoing p
