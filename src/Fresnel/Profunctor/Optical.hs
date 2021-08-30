module Fresnel.Profunctor.Optical
( Isoing
, Lensing
, Getting
, Prisming
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

class Profunctor p => Isoing p

instance Isoing (->)
instance Monad m => Isoing (Kleisli m)
instance Isoing (Forget r)
instance Isoing (Recall e)
instance Functor f => Isoing (Star f)
instance Functor f => Isoing (Costar f)
instance Functor f => Isoing (AffineStar f)
instance Isoing (Coexp s t)


class (Isoing p, Strong p) => Lensing p

instance Lensing (->)
instance Monad m => Lensing (Kleisli m)
instance Lensing (Forget r)
instance Functor f => Lensing (Star f)
instance Functor f => Lensing (AffineStar f)


class (Lensing p, Bicontravariant p, Cochoice p) => Getting p

instance Getting (Forget r)
instance (Contravariant f, Traversable f) => Getting (Star f)


class (Isoing p, Choice p) => Prisming p

instance Prisming (->)
instance Monad m => Prisming (Kleisli m)
instance Monoid r => Prisming (Forget r)
instance Prisming (Recall e)
instance Applicative f => Prisming (Star f)
instance Functor f => Prisming (AffineStar f)


class (Prisming p, Bifunctor p, Costrong p) => Reviewing p

instance Reviewing (Recall e)


class (Lensing p, Prisming p) => AffineTraversing p where

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

instance Monoid r => Traversing (Forget r)
instance Applicative f => Traversing (Star f)


class (AffineFolding p, Cochoice p, Traversing p) => Folding p

instance Monoid r => Folding (Forget r)


class (AffineTraversing p, Mapping p) => Setting p

instance Setting (->)
