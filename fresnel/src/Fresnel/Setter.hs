{-# LANGUAGE RankNTypes #-}
module Fresnel.Setter
( -- * Setters
  Setter
, Setter'
, IsSetter
  -- * Construction
, sets
, mapped
, contramapped
  -- * Elimination
, over
, (%~)
, set
, (.~)
, (+~)
) where

import Data.Functor.Contravariant
import Data.Profunctor.Mapping
import Fresnel.Optic
import Fresnel.Traversal.Internal (IsTraversal)

-- Setters

type Setter s t a b = forall p . IsSetter p => Optic p s t a b

type Setter' s a = Setter s s a a

class (IsTraversal p, Mapping p) => IsSetter p

instance IsSetter (->)


-- Construction

sets :: ((a -> b) -> (s -> t)) -> Setter s t a b
sets f = (f `roam`) -- written thus to placate hlint


mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap

contramapped :: Contravariant f => Setter (f a) (f b) b a
contramapped = sets contramap


-- Elimination

over, (%~) :: Setter s t a b -> (a -> b) -> (s -> t)
over o = o

(%~) = over

infixr 4 %~


set, (.~) :: Setter s t a b -> b -> s -> t
set o = over o . const

(.~) = set

infixr 4 .~, +~


(+~) :: Num a => Setter s t a a -> a -> s -> t
o +~ a = over o (a +)
