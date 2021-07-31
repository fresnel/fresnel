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
) where

import Data.Functor.Contravariant
import Data.Profunctor
import Fresnel.Optic

-- Getters

type Getter s a = forall p . (Contravariant (p a), Functor (p a), Strong p) => Optic' p s a


-- Construction

to :: (s -> a) -> Getter s a
to f = lmap f . phantom


getting :: (Functor (p a), Contravariant (p a), Functor (q s), Contravariant (q s)) => Optical p q s t a b -> Optical' p q s a
getting l f = phantom . l $ phantom f


-- Elimination

views :: Optic (Forget r) s t a b -> (a -> r) -> (s -> r)
views b = runForget . b . Forget

view :: Optic (Forget a) s t a b -> (s -> a)
view b = views b id

(^.) :: s -> Optic (Forget a) s t a b -> a
s ^. o = view o s

infixl 8 ^.
