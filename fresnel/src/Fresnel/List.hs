module Fresnel.List
( -- * Optics
  uncons_
, head_
, tail_
) where

import qualified Data.List as List
import           Fresnel.Optional
import           Fresnel.Prism

-- Optics

uncons_ :: Prism' [a] (a, [a])
uncons_ = prism' (uncurry (:)) List.uncons

head_ :: Optional' [a] a
head_ = uncons_.optional' (Just . fst) (\ (_,xs) x -> (x, xs))

tail_ :: Optional' [a] [a]
tail_ = uncons_.optional' (Just . snd) (\ (x, _) xs -> (x, xs))
