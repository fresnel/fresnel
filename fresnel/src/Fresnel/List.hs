module Fresnel.List
( -- * Optics
  uncons_
, head_
, tail_
) where

import qualified Data.List as List
import           Fresnel.Optional
import           Fresnel.Prism
import           Fresnel.Tuple

-- Optics

uncons_ :: Prism' [a] (a, [a])
uncons_ = prism' (uncurry (:)) List.uncons

head_ :: Optional' [a] a
head_ = uncons_.fst_

tail_ :: Optional' [a] [a]
tail_ = uncons_.snd_
