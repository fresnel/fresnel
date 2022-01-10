module Fresnel.List.NonEmpty
( -- * Optics
  uncons_
, head_
, tail_
) where

import qualified Data.List.NonEmpty as NE
import           Fresnel.Iso (Iso', iso)
import           Fresnel.Lens (Lens', lens)

-- Optics

uncons_ :: Iso' (NE.NonEmpty a) (a, Maybe (NE.NonEmpty a))
uncons_ = iso NE.uncons (uncurry (NE.:|) . fmap (foldMap NE.toList))

head_ :: Lens' (NE.NonEmpty a) a
head_ = lens NE.head (\ (_ NE.:|as) a' -> a' NE.:|as)

tail_ :: Lens' (NE.NonEmpty a) [a]
tail_ = lens NE.tail (\ (a NE.:|_) as' -> a NE.:|as')
