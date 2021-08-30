module Fresnel.Profunctor.Optical
( Isoing
, Lensing
, Getting
, Prisming
, Reviewing
, AffineTraversing
) where


import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Profunctor.Recall
import Fresnel.Profunctor.Coexp
import Data.Bifunctor

class Profunctor p => Isoing p

instance Isoing (->)
instance Isoing (Forget r)
instance Isoing (Recall e)
instance Isoing (Coexp s t)


class (Isoing p, Strong p) => Lensing p

instance Lensing (->)
instance Lensing (Forget r)


class (Lensing p, Bicontravariant p, Strong p) => Getting p

instance Getting (Forget r)


class (Isoing p, Choice p) => Prisming p

instance Prisming (->)
instance Prisming (Recall e)


class (Prisming p, Bifunctor p) => Reviewing p

instance Reviewing (Recall e) where


class (Lensing p, Prisming p) => AffineTraversing p where
