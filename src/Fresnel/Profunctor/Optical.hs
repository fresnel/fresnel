module Fresnel.Profunctor.Optical
( Isoing
, Lensing
, Getting
, Reviewing
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


class (Isoing p, Bicontravariant p, Strong p) => Getting p

instance Getting (Forget r)


class (Isoing p, Bifunctor p, Choice p) => Reviewing p

instance Reviewing (Recall e) where
