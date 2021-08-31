module Fresnel.Profunctor.Optical
( IsIso
, IsLens
, IsGetter
, IsPrism
, Reviewing
, AffineTraversing
, Traversing
, AffineFolding
, Folding
, Setting
) where


import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Profunctor.Recall
import Fresnel.Profunctor.Coexp
import Data.Bifunctor
import qualified Data.Profunctor.Traversing as Pro
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


class (IsPrism p, Bifunctor p, Costrong p) => Reviewing p

instance Reviewing (Recall e)


class (IsLens p, IsPrism p) => AffineTraversing p where

instance AffineTraversing (->)
instance Monad m => AffineTraversing (Kleisli m)
instance Monoid r => AffineTraversing (Forget r)
instance Applicative f => AffineTraversing (Star f)
instance Functor f => AffineTraversing (AffineStar f)


class (AffineTraversing p, Bicontravariant p) => AffineFolding p

instance Monoid r => AffineFolding (Forget r)
instance (Applicative f, Contravariant f) => AffineFolding (Star f)
instance (Functor f, Contravariant f) => AffineFolding (AffineStar f)


class (AffineTraversing p, Pro.Traversing p) => Traversing p

instance Traversing (->)
instance Monad m => Traversing (Kleisli m)
instance Monoid r => Traversing (Forget r)
instance Applicative f => Traversing (Star f)


class (AffineFolding p, Cochoice p, Traversing p) => Folding p

instance Monoid r => Folding (Forget r)


class (Traversing p, Mapping p) => Setting p

instance Setting (->)
