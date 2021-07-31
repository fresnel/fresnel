-- | Type synonyms for defining types of optics.
module Fresnel.Optic
( -- * Optics
  Optical
, Optic
, Optic'
) where

-- Optics

type Optical p q s t a b = (a `p` b) -> (s `q` t)


type Optic p s t a b = (a `p` b) -> (s `p` t)

type Optic' p s a = Optic p s s a a
