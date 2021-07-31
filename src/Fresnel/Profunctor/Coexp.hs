module Fresnel.Profunctor.Coexp
( Coexp(..)
) where

data Coexp r e b a = Coexp (e -> a) (b -> r)
  deriving (Functor)
