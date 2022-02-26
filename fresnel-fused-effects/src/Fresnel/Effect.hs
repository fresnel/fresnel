{-# LANGUAGE RankNTypes #-}
module Fresnel.Effect
( view
, views
, use
, uses
, assign
, modifying
, (.=)
, (%=)
, (<~)
, (+=)
, (-=)
, (*=)
, (//=)
) where

import           Control.Algebra
import qualified Control.Effect.Reader as R
import qualified Control.Effect.State as S
import qualified Fresnel.Getter as O
import qualified Fresnel.Setter as O

view :: Has (R.Reader r) sig m => O.Getter r a -> m a
view o = R.asks (O.view o)

views :: Has (R.Reader r) sig m => O.Getter r a -> (a -> b) -> m b
views o f = R.asks (O.views o f)


use :: Has (S.State s) sig m => O.Getter s a -> m a
use o = S.gets (O.view o)

uses :: Has (S.State s) sig m => O.Getter s a -> (a -> b) -> m b
uses o f = S.gets (O.views o f)


assign, (.=) :: Has (S.State s) sig m => O.Setter s s a b -> b -> m ()

assign o v = S.modify (O.set o v)

(.=) = assign

infix 4 .=


modifying, (%=) :: Has (S.State s) sig m => O.Setter s s a b -> (a -> b) -> m ()

modifying o f = S.modify (O.over o f)

(%=) = modifying

infix 4 %=



(<~) :: Has (S.State s) sig m => O.Setter s s a b -> m b -> m ()
o <~ m = m >>= assign o

infixr 2 <~


(+=), (-=), (*=) :: (Has (S.State s) sig m, Num a) => O.Setter s s a a -> a -> m ()
o += n = modifying o (+ n)
o -= n = modifying o (subtract n)
o *= n = modifying o (* n)

(//=) :: (Has (S.State s) sig m, Fractional a) => O.Setter s s a a -> a -> m ()
o //= n = modifying o (/ n)

infix 4 +=, -=, *=, //=
