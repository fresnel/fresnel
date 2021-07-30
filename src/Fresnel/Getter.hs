module Fresnel.Getter
( -- * Getters
  Getter
  -- * Construction
, to
) where

import Data.Functor.Contravariant
import Data.Profunctor
import Fresnel.Optic

-- Getters

type Getter s a = forall p . (Contravariant (p a), Functor (p a), Strong p) => Optic' p s a


-- Construction

to :: (s -> a) -> Getter s a
to f = lmap f . phantom
