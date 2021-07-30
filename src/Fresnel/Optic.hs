module Fresnel.Optic
( -- * Optics
  Optic
, Optic'
) where

-- Optics

type Optic p s t a b = (a `p` b) -> (s `p` t)

type Optic' p s a = Optic p s s a a
