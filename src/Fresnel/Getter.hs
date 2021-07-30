{-# LANGUAGE ConstraintKinds #-}
module Fresnel.Getter
( -- * Getters
  Getter
, IsGetter
  -- * Construction
, to
  -- * Elimination
, views
) where

import Data.Functor.Contravariant
import Data.Profunctor
import Fresnel.Optic

-- Getters

type Getter s a = forall p . IsGetter p a => Optic' p s a

type IsGetter p a = (Contravariant (p a), Functor (p a), Strong p)


-- Construction

to :: (s -> a) -> Getter s a
to f = lmap f . phantom


-- Elimination

views :: Getter s a -> (a -> r) -> (s -> r)
views b = runForget . b . Forget
