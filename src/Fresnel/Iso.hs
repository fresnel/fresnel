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

-- Isos

type Iso s t a b = forall p . Profunctor p => Optic p s t a b

type Iso' s a = Iso s s a a


-- Construction

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap


-- Elimination

withIso :: Iso s t a b -> (((s -> a) -> (b -> t) -> r) -> r)
withIso = withExchange . ($ Exchange id id)

data Exchange a b s t = Exchange (s -> a) (b -> t)
  deriving (Functor)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)

withExchange :: Exchange a b s t -> (((s -> a) -> (b -> t) -> r) -> r)
withExchange (Exchange sa bt) f = f sa bt


under :: Iso s t a b -> (t -> s) -> (b -> a)
under i = withIso i (\ f r -> (f .) . (. r))
