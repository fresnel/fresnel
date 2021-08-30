module Fresnel.Profunctor.Isoing
( -- * Iso constraints
  Isoing
) where


import Data.Profunctor
import Fresnel.Profunctor.Recall

-- Iso constraints

class Profunctor p => Isoing p

instance Isoing (->)
instance Isoing (Forget r)
instance Isoing (Recall e)
