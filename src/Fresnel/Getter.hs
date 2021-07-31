module Fresnel.Getter
( -- * Getters
  Getter
  -- * Construction
, to
, getting
  -- * Elimination
, views
, view
, (^.)
  -- * Utilities
, rphantom
) where

import Data.Functor.Contravariant
import Data.Profunctor
import Fresnel.Optic

-- Getters

type Getter s a = forall p . (Contravariant (p a), Strong p) => Optic' p s a


-- Construction

to :: (s -> a) -> Getter s a
to f = lmap f . rphantom


getting :: (Profunctor p, Profunctor q, Contravariant (p a), Contravariant (q s)) => Optical p q s t a b -> Optical' p q s a
getting l f = rphantom . l $ rphantom f


-- Elimination

views :: Optic (Forget r) s t a b -> (a -> r) -> (s -> r)
views b = runForget . b . Forget

view :: Optic (Forget a) s t a b -> (s -> a)
view b = views b id

(^.) :: s -> Optic (Forget a) s t a b -> a
s ^. o = view o s

infixl 8 ^.


-- Utilities

rphantom :: (Profunctor p, Contravariant (p a)) => p a b -> p a c
rphantom p = contramap (const ()) (rmap (const ()) p)
