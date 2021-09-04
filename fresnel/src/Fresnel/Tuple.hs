module Fresnel.Tuple
( -- * Lenses
  fst_
, snd_
) where

import Fresnel.Lens

-- Lenses

fst_ :: Lens (a, b) (a', b) a a'
fst_ = lens fst (\ s a' -> (a', snd s))

snd_ :: Lens (a, b) (a, b') b b'
snd_ = lens snd (\ s b' -> (fst s, b'))
