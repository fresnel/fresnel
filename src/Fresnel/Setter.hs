{-# LANGUAGE RankNTypes #-}
module Fresnel.Setter
( -- * Setters
  Setter
, Setter'
  -- * Construction
, sets
  -- * Elimination
, over
, (%~)
, set
, (.~)
) where

import Data.Profunctor.Mapping
import Fresnel.Optic
import Fresnel.Profunctor.Optical

-- Setters

type Setter s t a b = forall p . Setting p => Optic p s t a b

type Setter' s a = Setter s s a a


-- Construction

sets :: ((a -> b) -> (s -> t)) -> Setter s t a b
sets f = (f `roam`) -- written thus to placate hlint


-- Elimination

over, (%~) :: Setter s t a b -> (a -> b) -> (s -> t)
over o = o

(%~) = over

infixr 4 %~


set, (.~) :: Setter s t a b -> b -> s -> t
set o = over o . const

(.~) = set

infixr 4 .~
