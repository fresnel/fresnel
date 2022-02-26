{-# LANGUAGE Rank2Types #-}
module Fresnel.Effect
( assign
, (.=)
) where

import Control.Effect.State
import Fresnel.Setter

assign, (.=) :: Has (State s) sig m => Setter s s a b -> b -> m ()

assign o v = modify (set o v)

(.=) = assign

infix 4 .=
