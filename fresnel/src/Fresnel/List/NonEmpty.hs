module Fresnel.List.NonEmpty
( -- * Optics
  nonEmpty_
, uncons_
, head_
, tail_
) where

import qualified Data.List.NonEmpty as NE
import           Fresnel.Iso (Iso, from, iso)
import           Fresnel.Lens (Lens')
import           Fresnel.Tuple (fst_, snd_)

-- Optics

nonEmpty_ :: Iso [a] [b] (Maybe (NE.NonEmpty a)) (Maybe (NE.NonEmpty b))
nonEmpty_ = iso NE.nonEmpty (foldMap NE.toList)

uncons_ :: Iso (NE.NonEmpty a) (NE.NonEmpty b) (a, Maybe (NE.NonEmpty a)) (b, Maybe (NE.NonEmpty b))
uncons_ = iso NE.uncons (uncurry (NE.:|) . fmap (foldMap NE.toList))

head_ :: Lens' (NE.NonEmpty a) a
head_ = uncons_.fst_

tail_ :: Lens' (NE.NonEmpty a) [a]
tail_ = uncons_.snd_.from nonEmpty_
