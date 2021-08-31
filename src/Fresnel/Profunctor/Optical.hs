module Fresnel.Profunctor.Optical
( IsIso
, IsLens
, IsGetter
, IsPrism
, IsReview
, IsAffineTraversal
, IsTraversal
, IsAffineFold
, Folding
, Setting
) where


import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Profunctor.Recall
import Fresnel.Profunctor.Coexp
import Data.Bifunctor
import Data.Profunctor.Traversing
import Fresnel.Profunctor.AffineStar
import Data.Functor.Contravariant
import Control.Arrow

class Profunctor p => IsIso p

instance IsIso (->)
instance Monad m => IsIso (Kleisli m)
instance IsIso (Forget r)
instance IsIso (Recall e)
instance Functor f => IsIso (Star f)
instance Functor f => IsIso (Costar f)
instance Functor f => IsIso (AffineStar f)
instance IsIso (Coexp s t)


class (IsIso p, Strong p) => IsLens p

instance IsLens (->)
instance Monad m => IsLens (Kleisli m)
instance IsLens (Forget r)
instance Functor f => IsLens (Star f)
instance Functor f => IsLens (AffineStar f)


class (IsLens p, Bicontravariant p, Cochoice p) => IsGetter p

instance IsGetter (Forget r)
instance (Contravariant f, Traversable f) => IsGetter (Star f)


class (IsIso p, Choice p) => IsPrism p

instance IsPrism (->)
instance Monad m => IsPrism (Kleisli m)
instance Monoid r => IsPrism (Forget r)
instance IsPrism (Recall e)
instance Applicative f => IsPrism (Star f)
instance Functor f => IsPrism (AffineStar f)


class (IsPrism p, Bifunctor p, Costrong p) => IsReview p

instance IsReview (Recall e)


class (IsLens p, IsPrism p) => IsAffineTraversal p where

instance IsAffineTraversal (->)
instance Monad m => IsAffineTraversal (Kleisli m)
instance Monoid r => IsAffineTraversal (Forget r)
instance Applicative f => IsAffineTraversal (Star f)
instance Functor f => IsAffineTraversal (AffineStar f)


class (IsAffineTraversal p, Bicontravariant p) => IsAffineFold p

instance Monoid r => IsAffineFold (Forget r)
instance (Applicative f, Contravariant f) => IsAffineFold (Star f)
instance (Functor f, Contravariant f) => IsAffineFold (AffineStar f)


class (IsAffineTraversal p, Traversing p) => IsTraversal p

instance IsTraversal (->)
instance Monad m => IsTraversal (Kleisli m)
instance Monoid r => IsTraversal (Forget r)
instance Applicative f => IsTraversal (Star f)


class (IsAffineFold p, IsTraversal p, Cochoice p) => Folding p

instance Monoid r => Folding (Forget r)


class (IsTraversal p, Mapping p) => Setting p

instance Setting (->)
