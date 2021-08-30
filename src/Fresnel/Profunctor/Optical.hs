module Fresnel.Profunctor.Optical
( Isoing
, Lensing
, Getting
, Prisming
, Reviewing
, AffineTraversing
, Traversing(..)
, AffineFolding
) where


import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Profunctor.Recall
import Fresnel.Profunctor.Coexp
import Data.Bifunctor
import Data.Profunctor.Traversing
import Fresnel.Profunctor.AffineStar
import Data.Functor.Contravariant

class Profunctor p => Isoing p

instance Isoing (->)
instance Isoing (Forget r)
instance Isoing (Recall e)
instance Functor f => Isoing (Star f)
instance Functor f => Isoing (Costar f)
instance Functor f => Isoing (AffineStar f)
instance Isoing (Coexp s t)


class (Isoing p, Strong p) => Lensing p

instance Lensing (->)
instance Lensing (Forget r)
instance Functor f => Lensing (Star f)
instance Functor f => Lensing (AffineStar f)


class (Lensing p, Bicontravariant p) => Getting p

instance Getting (Forget r)


class (Isoing p, Choice p) => Prisming p

instance Prisming (->)
instance Prisming (Recall e)
instance Applicative f => Prisming (Star f)
instance Functor f => Prisming (AffineStar f)


class (Prisming p, Bifunctor p) => Reviewing p

instance Reviewing (Recall e)


class (Lensing p, Prisming p) => AffineTraversing p where

instance AffineTraversing (->)
instance Applicative f => AffineTraversing (Star f)
instance Functor f => AffineTraversing (AffineStar f)


class (AffineTraversing p, Bicontravariant p) => AffineFolding p

instance (Functor f, Contravariant f) => AffineFolding (AffineStar f)
