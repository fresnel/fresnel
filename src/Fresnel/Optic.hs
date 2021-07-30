module Fresnel.Optic
( -- * Optics
  Optic
) where

-- Optics

type Optic p s t a b = (a `p` b) -> (s `p` t)
