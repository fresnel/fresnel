module Fresnel.Iso
( -- * Isos
  Iso
, Iso'
  -- * Construction
, iso
  -- * Elimination
, withIso
, under
) where

import Data.Profunctor
import Fresnel.Optic
import Fresnel.Profunctor.Coexp

-- Isos

type Iso s t a b = forall p . Profunctor p => Optic p s t a b

type Iso' s a = Iso s s a a


-- Construction

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap


-- Elimination

withIso :: Iso s t a b -> (((s -> a) -> (b -> t) -> r) -> r)
withIso i = withCoexp (i (Coexp id id)) . flip


under :: Iso s t a b -> (t -> s) -> (b -> a)
under i = withIso i (\ f r -> (f .) . (. r))
