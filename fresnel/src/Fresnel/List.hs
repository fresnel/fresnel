module Fresnel.List
( -- * Optics
  uncons_
) where

import qualified Data.List as List
import           Fresnel.Prism

-- Optics

uncons_ :: Prism' [a] (a, [a])
uncons_ = prism' (uncurry (:)) List.uncons
