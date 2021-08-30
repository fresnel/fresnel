module Fresnel.Profunctor.Optical
( Isoing
, Lensing
, Getting
, Prisming
, Reviewing
, AffineTraversing
, Traversing(..)
) where


import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Profunctor.Recall
import Fresnel.Profunctor.Coexp
import Data.Bifunctor
import Data.Profunctor.Traversing

class Profunctor p => Isoing p

instance Isoing (->)
instance Isoing (Forget r)
instance Isoing (Recall e)
instance Functor f => Isoing (Star f)
instance Functor f => Isoing (Costar f)
instance Isoing (Coexp s t)


class (Isoing p, Strong p) => Lensing p

instance Lensing (->)
instance Lensing (Forget r)
instance Functor f => Lensing (Star f)


class (Lensing p, Bicontravariant p, Strong p) => Getting p

instance Getting (Forget r)


class (Isoing p, Choice p) => Prisming p

instance Prisming (->)
instance Prisming (Recall e)
instance Applicative f => Prisming (Star f)


class (Prisming p, Bifunctor p) => Reviewing p

instance Reviewing (Recall e) where


class (Lensing p, Prisming p) => AffineTraversing p where

instance AffineTraversing (->)
instance Applicative f => AffineTraversing (Star f)
