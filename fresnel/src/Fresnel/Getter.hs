{-# LANGUAGE RankNTypes #-}
module Fresnel.Getter
( -- * Getters
  Getter
, IsGetter
  -- * Construction
, to
, getting
  -- * Elimination
, views
, view
, (^.)
) where

import Data.Functor.Contravariant (Contravariant)
import Data.Profunctor
import Data.Profunctor.Unsafe ((#.), (.#))
import Fresnel.Bifunctor.Contravariant
import Fresnel.Lens (IsLens)
import Fresnel.Optic
import Fresnel.Profunctor.OptionalStar (OptionalStar)

-- Getters

type Getter s a = forall p . IsGetter p => Optic' p s a

class (IsLens p, Bicontravariant p, Cochoice p) => IsGetter p

instance IsGetter (Forget r)
instance (Contravariant f, Traversable f) => IsGetter (Star f)
instance (Contravariant f, Traversable f) => IsGetter (OptionalStar f)


-- Construction

to :: (s -> a) -> Getter s a
to f = lmap f . rphantom


getting :: (Profunctor p, Bicontravariant p) => Optic p s t a b -> Optic' p s a
getting l f = rphantom . l $ rphantom f


-- Elimination

views :: Getter s a -> (a -> r) -> (s -> r)
views b = runForget #. b .# Forget

view :: Getter s a -> (s -> a)
view b = views b id

(^.) :: s -> Getter s a -> a
s ^. o = view o s

infixl 8 ^.
