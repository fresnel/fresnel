module Fresnel.List
( -- * Optics
  uncons_
, head_
) where

import qualified Data.List as List
import           Fresnel.Optional
import           Fresnel.Prism

-- Optics

uncons_ :: Prism' [a] (a, [a])
uncons_ = prism' (uncurry (:)) List.uncons

head_ :: Optional' [a] a
head_ = uncons_.optional' (Just . fst) (\ (_,xs) x -> (x, xs))
