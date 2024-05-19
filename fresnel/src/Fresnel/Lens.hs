{-# LANGUAGE RankNTypes #-}
module Fresnel.Lens
( -- * Lenses
  Lens
, Lens'
, IsLens
  -- * Construction
, lens
  -- * Elimination
, withLens
  -- * Combinators
, choosing
, chosen
, alongside
, inside
, devoid
, united
  -- * Unpacked
, UnpackedLens(..)
, unpackedLens
) where

import Control.Arrow ((&&&), (***))
import Data.Bifunctor (Bifunctor(..))
import Data.Profunctor
import Data.Profunctor.Rep (Corepresentable(..))
import Data.Profunctor.Sieve (Cosieve(..))
import Data.Void (Void, absurd)
import Fresnel.Getter (getting, view)
import Fresnel.Iso.Internal (IsIso)
import Fresnel.Lens.Internal (IsLens)
import Fresnel.Optic
import Fresnel.Setter (set)

-- Lenses

type Lens s t a b = forall p . IsLens p => Optic p s t a b

type Lens' s a = Lens s s a a


-- Construction

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = dimap (id &&& get) (uncurry set) . second'


-- Elimination

withLens :: Lens s t a b -> (((s -> a) -> (s -> b -> t) -> r) -> r)
withLens o = withUnpackedLens (o (unpackedLens id (const id)))


-- Combinators

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l r = lens
  (either (view (getting l)) (view (getting r)))
  (\ e b -> bimap (set l b) (set r b) e)

chosen :: Lens (Either a a) (Either b b) a b
chosen = choosing id id

alongside :: Lens s1 t1 a1 b1 -> Lens s2 t2 a2 b2 -> Lens (s1, s2) (t1, t2) (a1, a2) (b1, b2)
alongside o1 o2 = withLens o1 $ \ get1 set1 -> withLens o2 $ \ get2 set2 ->
  lens (get1 *** get2) (uncurry (***) . (set1 *** set2))

inside :: Corepresentable p => Lens s t a b -> Lens (p e s) (p e t) (p e a) (p e b)
inside o = lens
  (\ s -> cotabulate (view (getting o) . cosieve s))
  (\ s b -> cotabulate (\ e -> set o (cosieve b e) (cosieve s e)))

devoid :: Lens Void Void a b
devoid = lens absurd const

united :: Lens' a ()
united = lens (const ()) const


-- Unpacked

-- | A 'Lens' unpacked into the get & set functions it was constructed from.
newtype UnpackedLens a b s t = UnpackedLens { withUnpackedLens :: forall r . ((s -> a) -> (s -> b -> t) -> r) -> r }

instance Profunctor (UnpackedLens a b) where
  dimap f g (UnpackedLens r) = r $ \ get set -> unpackedLens (get . f) (rmap g . set . f)

instance Strong (UnpackedLens a b) where
  first' (UnpackedLens r) = r $ \ get set -> unpackedLens (get . fst) (\ (a, c) b -> (set a b, c))

instance IsIso (UnpackedLens a b)
instance IsLens (UnpackedLens a b)


unpackedLens :: (s -> a) -> (s -> b -> t) -> UnpackedLens a b s t
unpackedLens get set = UnpackedLens (\ k -> k get set)
