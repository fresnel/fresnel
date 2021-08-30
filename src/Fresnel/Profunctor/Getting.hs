module Fresnel.Profunctor.Getting
( -- * Getter constraints
  Getting
) where

import Fresnel.Bifunctor.Contravariant
import Data.Profunctor

-- Getter constraints

class (Bicontravariant p, Strong p) => Getting p
