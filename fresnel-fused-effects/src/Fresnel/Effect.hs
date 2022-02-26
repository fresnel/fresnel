{-# LANGUAGE Rank2Types #-}
module Fresnel.Effect
( view
, assign
, modifying
, (.=)
, (%=)
) where

import           Control.Algebra
import qualified Control.Effect.Reader as R
import qualified Control.Effect.State as S
import qualified Fresnel.Getter as O
import qualified Fresnel.Setter as O

view :: Has (R.Reader r) sig m => O.Getter r a -> m a
view o = R.asks (O.view o)


assign, (.=) :: Has (S.State s) sig m => O.Setter s s a b -> b -> m ()

assign o v = S.modify (O.set o v)

(.=) = assign

infix 4 .=


modifying, (%=) :: Has (S.State s) sig m => O.Setter s s a b -> (a -> b) -> m ()

modifying o f = S.modify (O.over o f)

(%=) = modifying

infix 4 %=
