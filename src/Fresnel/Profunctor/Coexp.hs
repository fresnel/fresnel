module Fresnel.Profunctor.Coexp
( -- * Coexponential profunctor
  Coexp(..)
) where

import Data.Profunctor

-- Coexponential

data Coexp r e b a = Coexp { recall :: e -> a, forget :: b -> r }
  deriving (Functor)

instance Profunctor (Coexp r e) where
  dimap f g (Coexp bt sa) = Coexp (g . bt) (sa . f)
