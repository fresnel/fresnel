module Fresnel.Profunctor.Getting
( -- * Getter constraints
  Getting
) where

import Fresnel.Bifunctor.Contravariant
import Data.Profunctor
import Fresnel.Profunctor.Isoing

-- Getter constraints

class (Isoing p, Bicontravariant p, Strong p) => Getting p

instance Getting (Forget r)
