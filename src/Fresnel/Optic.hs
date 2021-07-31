-- | Type synonyms for defining types of optics.
module Fresnel.Optic
( -- * Optics
  Optical
, Optical'
, Optic
, Optic'
) where

-- Optics

type Optical p q s t a b = (a `p` b) -> (s `q` t)

type Optical' p q s a = Optical p q s s a a


type Optic p s t a b = Optical p p s t a b

type Optic' p s a = Optic p s s a a
