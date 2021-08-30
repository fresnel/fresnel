module Fresnel.Profunctor.Isoing
( -- * Iso constraints
  Isoing
) where


import Data.Profunctor

-- Iso constraints

class Profunctor p => Isoing p

instance Isoing (->)
instance Isoing (Forget r)
