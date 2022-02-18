{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Fresnel.Prism
( -- * Prisms
  Prism
, Prism'
, IsPrism
  -- * Construction
, prism
, prism'
  -- * Elimination
, withPrism
, matching
, matching'
, is
, isn't
  -- * Relations
, only
, nearly
  -- * Combinators
, without
, below
, aside
  -- * Unpacked
, UnpackedPrism(..)
, unpackedPrism
) where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor(..))
import Data.Profunctor
import Fresnel.Iso.Internal (IsIso)
import Fresnel.Optic
import Fresnel.Optional (is, isn't, matching, matching')
import Fresnel.Prism.Internal (IsPrism)

-- Prisms

type Prism s t a b = forall p . IsPrism p => Optic p s t a b

type Prism' s a = Prism s s a a


-- Construction

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism inj prj = dimap prj (either id inj) . right'

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' inj prj = prism inj (\ s -> maybe (Left s) Right (prj s))


-- Elimination

withPrism :: Prism s t a b -> (((b -> t) -> (s -> Either t a) -> r) -> r)
withPrism o = withUnpackedPrism (o (unpackedPrism id Right))


-- Relations

only :: Eq a => a -> Prism' a ()
only a = nearly a (a ==)

nearly :: a -> (a -> Bool) -> Prism' a ()
nearly a p = prism' (const a) (guard . p)


-- Combinators

without :: Prism s1 t1 a1 b1 -> Prism s2 t2 a2 b2 -> Prism (Either s1 s2) (Either t1 t2) (Either a1 a2) (Either b1 b2)
without o1 o2 = withPrism o1 $ \ inj1 prj1 -> withPrism o2 $ \ inj2 prj2 ->
  prism (bimap inj1 inj2) (either (bimap Left Left . prj1) (bimap Right Right . prj2))

below :: Traversable f => Prism' s a -> Prism' (f s) (f a)
below o = withPrism o $ \ inj prj -> prism (fmap inj) $ \ s -> first (const s) (traverse prj s)

aside :: Prism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside o = withPrism o $ \ inj prj -> prism (fmap inj) $ \ (e, s) -> bimap (e,) (e,) (prj s)


-- Unpacked

newtype UnpackedPrism a b s t = UnpackedPrism { withUnpackedPrism :: forall r . ((b -> t) -> (s -> Either t a) -> r) -> r }

instance Functor (UnpackedPrism a b s) where
  fmap = rmap

instance Profunctor (UnpackedPrism a b) where
  dimap f g (UnpackedPrism r) = r $ \ inj prj -> unpackedPrism (g . inj) (either (Left . g) Right . prj . f)

instance Choice (UnpackedPrism a b) where
  left' (UnpackedPrism r) = r $ \ inj prj -> unpackedPrism (Left . inj) (either (either (Left . Left) Right . prj) (Left . Right))

instance IsIso (UnpackedPrism a b)
instance IsPrism (UnpackedPrism a b)


unpackedPrism :: (b -> t) -> (s -> Either t a) -> UnpackedPrism a b s t
unpackedPrism inj prj = UnpackedPrism (\ k -> k inj prj)
