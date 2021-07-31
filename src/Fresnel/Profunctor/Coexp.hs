module Fresnel.Profunctor.Coexp
( -- * Coexponential profunctor
  Coexp(..)
) where

import Data.Profunctor

-- Coexponential

data Coexp r e b a = Coexp { recall :: e -> a, forget :: b -> r }
  deriving (Functor)

instance Profunctor (Coexp r e) where
  dimap f g c = Coexp (g . recall c) (forget c . f)
