module Fresnel.List.NonEmpty
( -- * Optics
  uncons_
, head_
, tail_
) where

import qualified Data.List.NonEmpty as NE
import           Fresnel.Iso (Iso, iso)
import           Fresnel.Lens (Lens', lens)
import           Fresnel.Tuple (fst_)

-- Optics

uncons_ :: Iso (NE.NonEmpty a) (NE.NonEmpty b) (a, Maybe (NE.NonEmpty a)) (b, Maybe (NE.NonEmpty b))
uncons_ = iso NE.uncons (uncurry (NE.:|) . fmap (foldMap NE.toList))

head_ :: Lens' (NE.NonEmpty a) a
head_ = uncons_.fst_

tail_ :: Lens' (NE.NonEmpty a) [a]
tail_ = lens NE.tail (\ (a NE.:|_) as' -> a NE.:|as')
