{-# LANGUAGE RankNTypes #-}
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
  -- * Combinators
, without
  -- * Unpacked
, UnpackedPrism(..)
, unpackedPrism
) where

import Control.Arrow (Kleisli)
import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Profunctor
import Fresnel.Iso.Internal (IsIso)
import Fresnel.Optic
import Fresnel.Profunctor.OptionalStar (OptionalStar)
import Fresnel.Profunctor.Recall (Recall)

-- Prisms

type Prism s t a b = forall p . IsPrism p => Optic p s t a b

type Prism' s a = Prism s s a a

class (IsIso p, Choice p) => IsPrism p

instance IsPrism (->)
instance Monad m => IsPrism (Kleisli m)
instance Monoid r => IsPrism (Forget r)
instance IsPrism (Recall e)
instance Applicative f => IsPrism (Star f)
instance Functor f => IsPrism (OptionalStar f)


-- Construction

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism inj prj = dimap prj (either id inj) . right'

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' inj prj = prism inj (\ s -> maybe (Left s) Right (prj s))


-- Elimination

withPrism :: Prism s t a b -> (((b -> t) -> (s -> Either t a) -> r) -> r)
withPrism o = withUnpackedPrism (o (unpackedPrism id Right))

matching :: Prism s t a b -> (s -> Either t a)
matching o = withPrism o (const id)

matching' :: Prism s t a b -> (s -> Maybe a)
matching' o = withPrism o (\ _ prj -> either (const Nothing) Just . prj)

is :: Prism s t a b -> (s -> Bool)
is o = either (const False) (const True) . matching o

isn't :: Prism s t a b -> (s -> Bool)
isn't o = either (const True) (const False) . matching o


-- Relations

only :: Eq a => a -> Prism' a ()
only a = prism' (const a) (guard . (== a))


-- Combinators

without :: Prism s1 t1 a1 b1 -> Prism s2 t2 a2 b2 -> Prism (Either s1 s2) (Either t1 t2) (Either a1 a2) (Either b1 b2)
without o1 o2 = withPrism o1 $ \ inj1 prj1 -> withPrism o2 $ \ inj2 prj2 ->
  prism (bimap inj1 inj2) (either (bimap Left Left . prj1) (bimap Right Right . prj2))


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
