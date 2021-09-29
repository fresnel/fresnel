{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Fresnel.Optional
( -- * Optionals
  Optional
, Optional'
, IsOptional
  -- * Construction
, optional
, optional'
  -- * Elimination
, matching
, matching'
, withOptional
, traverseOf
, is
  -- * Unpacked
, UnpackedOptional(..)
, unpackedOptional
) where

import Control.Arrow (Kleisli)
import Data.Bifunctor
import Data.Maybe (isJust)
import Data.Profunctor
import Fresnel.Iso (IsIso)
import Fresnel.Lens (IsLens)
import Fresnel.Optic
import Fresnel.Prism (IsPrism)
import Fresnel.Profunctor.OptionalStar

-- Optional traversals

type Optional s t a b = forall p . IsOptional p => Optic p s t a b

type Optional' s a = Optional s s a a

class (IsLens p, IsPrism p) => IsOptional p where

instance IsOptional (->)
instance Monad m => IsOptional (Kleisli m)
instance Monoid r => IsOptional (Forget r)
instance Applicative f => IsOptional (Star f)
instance Functor f => IsOptional (OptionalStar f)


-- Construction

optional :: (s -> Either t a) -> (s -> b -> t) -> Optional s t a b
optional prj set = dimap
  (\ s -> (prj s, set s))
  (\ (e, f) -> either id f e)
  . first' . right'

optional' :: (s -> Maybe a) -> (s -> b -> s) -> Optional s s a b
optional' prj = optional (\ s -> maybe (Left s) Right (prj s))


-- Elimination

matching :: Optional s t a b -> (s -> Either t a)
matching o = withOptional o const

matching' :: Optional s t a b -> (s -> Maybe a)
matching' o = withOptional o (\ prj _ -> either (const Nothing) Just . prj)

withOptional :: Optional s t a b -> (((s -> Either t a) -> (s -> b -> t) -> r) -> r)
withOptional o = withUnpackedOptional (o (unpackedOptional Right (const id)))

traverseOf :: Functor f => Optional s t a b -> (forall r . r -> f r) -> (a -> f b) -> (s -> f t)
traverseOf o point = runOptionalStar . o . optionalStar point

is :: Optional s t a b -> (s -> Bool)
is o = isJust . matching' o


-- Unpacked

newtype UnpackedOptional a b s t = UnpackedOptional { withUnpackedOptional :: forall r . ((s -> Either t a) -> (s -> b -> t) -> r) -> r }

instance Profunctor (UnpackedOptional a b) where
  dimap f g (UnpackedOptional r) = r $ \ prj set -> unpackedOptional (either (Left . g) Right . prj . f) (rmap g . set . f)

instance Strong (UnpackedOptional a b) where
  first'  (UnpackedOptional r) = r $ \ prj set -> unpackedOptional (\ (a, c) -> first (,c) (prj a)) (\ (a, c) b -> (set a b, c))
  second' (UnpackedOptional r) = r $ \ prj set -> unpackedOptional (\ (c, a) -> first (c,) (prj a)) (\ (c, a) b -> (c, set a b))

instance Choice (UnpackedOptional a b) where
  left' (UnpackedOptional r) = r $ \ prj set -> unpackedOptional (either (either (Left . Left) Right . prj) (Left . Right)) (\ e b -> first (`set` b) e)
  right' (UnpackedOptional r) = r $ \ prj set -> unpackedOptional (either (Left . Left) (either (Left . Right) Right . prj)) (\ e b -> fmap (`set` b) e)

instance IsIso (UnpackedOptional a b)
instance IsLens (UnpackedOptional a b)
instance IsPrism (UnpackedOptional a b)
instance IsOptional (UnpackedOptional a b)


unpackedOptional :: (s -> Either t a) -> (s -> b -> t) -> UnpackedOptional a b s t
unpackedOptional prj set = UnpackedOptional (\ k -> k prj set)
