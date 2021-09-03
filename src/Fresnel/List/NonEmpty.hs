module Fresnel.List.NonEmpty
( -- * Optics
  head_
, tail_
) where

import qualified Data.List.NonEmpty as NE
import           Fresnel.Lens (Lens', lens)

-- Optics

head_ :: Lens' (NE.NonEmpty a) a
head_ = lens NE.head (\ (_ NE.:|as) a' -> a' NE.:|as)

tail_ :: Lens' (NE.NonEmpty a) [a]
tail_ = lens NE.tail (\ (a NE.:|_) as' -> a NE.:|as')
