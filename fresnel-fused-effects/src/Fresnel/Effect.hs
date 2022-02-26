{-# LANGUAGE Rank2Types #-}
module Fresnel.Effect
( assign
, modifying
, (.=)
, (%=)
) where

import qualified Control.Effect.State as E
import qualified Fresnel.Setter as O

assign, (.=) :: E.Has (E.State s) sig m => O.Setter s s a b -> b -> m ()

assign o v = E.modify (O.set o v)

(.=) = assign

infix 4 .=


modifying, (%=) :: E.Has (E.State s) sig m => O.Setter s s a b -> (a -> b) -> m ()

modifying o f = E.modify (O.over o f)

(%=) = modifying

infix 4 %=
