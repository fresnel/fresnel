module Fresnel.Profunctor.Reviewing
( -- * Review constraints
  Reviewing
) where

import Fresnel.Profunctor.Isoing
import Fresnel.Profunctor.Recall
import Data.Bifunctor
import Data.Profunctor

-- Review constraints

class (Isoing p, Bifunctor p, Choice p) => Reviewing p

instance Reviewing (Recall e) where
