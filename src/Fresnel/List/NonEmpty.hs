module Fresnel.List.NonEmpty
( -- * Optics
  head_
) where

import qualified Data.List.NonEmpty as NE
import           Fresnel.Lens (Lens', lens)

-- Optics

head_ :: Lens' (NE.NonEmpty a) a
head_ = lens NE.head (\ (_ NE.:|as) a' -> a' NE.:|as)
