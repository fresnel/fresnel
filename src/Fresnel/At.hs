{-# LANGUAGE RankNTypes #-}
module Fresnel.At
( -- * Updateable collections
  At(..)
  -- * Indexable collections
, module Fresnel.Ixed
) where

import Fresnel.Ixed
import Fresnel.Lens

class Ixed c => At c where
  at :: Index c -> Lens' c (Maybe (IxValue c))
