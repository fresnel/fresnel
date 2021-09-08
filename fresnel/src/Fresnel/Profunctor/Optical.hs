module Fresnel.Profunctor.Optical
( IsIso
, IsLens
) where


import Control.Arrow
import Data.Profunctor
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
