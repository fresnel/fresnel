module Fresnel.Profunctor.Optical
( IsIso
, IsLens
, IsGetter
, IsPrism
, IsReview
, IsOptional
, IsTraversal
, IsOptionalFold
, IsFold
, IsSetter
) where


import Control.Arrow
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Profunctor.Traversing
import Fresnel.Bifunctor.Contravariant
import Fresnel.Profunctor.Coexp
import Fresnel.Profunctor.OptionalStar
import Fresnel.Profunctor.Recall

class Profunctor p => IsIso p

instance IsIso (->)
instance Monad m => IsIso (Kleisli m)
instance IsIso (Forget r)
instance IsIso (Recall e)
instance Functor f => IsIso (Star f)
instance Functor f => IsIso (Costar f)
instance Functor f => IsIso (OptionalStar f)
instance IsIso (Coexp s t)


class (IsIso p, Strong p) => IsLens p

instance IsLens (->)
instance Monad m => IsLens (Kleisli m)
instance IsLens (Forget r)
instance Functor f => IsLens (Star f)
instance Functor f => IsLens (OptionalStar f)


class (IsLens p, Bicontravariant p, Cochoice p) => IsGetter p

instance IsGetter (Forget r)
instance (Contravariant f, Traversable f) => IsGetter (Star f)
instance (Contravariant f, Traversable f) => IsGetter (OptionalStar f)


class (IsIso p, Choice p) => IsPrism p

instance IsPrism (->)
instance Monad m => IsPrism (Kleisli m)
instance Monoid r => IsPrism (Forget r)
instance IsPrism (Recall e)
instance Applicative f => IsPrism (Star f)
instance Functor f => IsPrism (OptionalStar f)


class (IsPrism p, Bifunctor p, Costrong p) => IsReview p

instance IsReview (Recall e)


class (IsLens p, IsPrism p) => IsOptional p where

instance IsOptional (->)
instance Monad m => IsOptional (Kleisli m)
instance Monoid r => IsOptional (Forget r)
instance Applicative f => IsOptional (Star f)
instance Functor f => IsOptional (OptionalStar f)


class (IsOptional p, IsGetter p) => IsOptionalFold p

instance Monoid r => IsOptionalFold (Forget r)
instance (Applicative f, Traversable f, Contravariant f) => IsOptionalFold (Star f)
instance (Traversable f, Contravariant f) => IsOptionalFold (OptionalStar f)


class (IsOptional p, Traversing p) => IsTraversal p

instance IsTraversal (->)
instance Monad m => IsTraversal (Kleisli m)
instance Monoid r => IsTraversal (Forget r)
instance Applicative f => IsTraversal (Star f)


class (IsOptionalFold p, IsTraversal p) => IsFold p

instance Monoid r => IsFold (Forget r)


class (IsTraversal p, Mapping p) => IsSetter p

instance IsSetter (->)
