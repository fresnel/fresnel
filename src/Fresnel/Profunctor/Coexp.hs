module Fresnel.Profunctor.Coexp
( Coexp(..)
) where

import Data.Profunctor

data Coexp r e b a = Coexp (e -> a) (b -> r)
  deriving (Functor)

instance Profunctor (Coexp r e) where
  dimap f g (Coexp bt sa) = Coexp (g . bt) (sa . f)
