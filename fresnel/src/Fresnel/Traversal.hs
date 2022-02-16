{-# LANGUAGE RankNTypes #-}
module Fresnel.Traversal
( -- * Traversals
  Traversal
, Traversal'
, IsTraversal
  -- * Construction
, traversed
, backwards
  -- * Elimination
, traverseOf
, forOf
, sequenceOf
, transposeOf
, mapAccumLOf
, mapAccumROf
) where

import Control.Applicative (ZipList(..))
import Control.Monad.Trans.State
import Data.Profunctor
import Data.Profunctor.Traversing (Traversing(..))
import Data.Profunctor.Unsafe ((#.))
import Fresnel.Functor.Backwards
import Fresnel.Optic
import Fresnel.Traversal.Internal (IsTraversal)

-- Traversals

type Traversal s t a b = forall p . IsTraversal p => Optic p s t a b

type Traversal' s a = Traversal s s a a


-- Construction

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = wander traverse

backwards :: Traversal s t a b -> Traversal s t a b
backwards o = wander (\ f -> forwards . traverseOf o (Backwards . f))


-- Elimination

traverseOf :: Applicative f => Traversal s t a b -> ((a -> f b) -> (s -> f t))
traverseOf o = runStar . o . Star

forOf :: Applicative f => Traversal s t a b -> (s -> (a -> f b) -> f t)
forOf o = flip (traverseOf o)

sequenceOf :: Applicative f => Traversal s t (f b) b -> (s -> f t)
sequenceOf o = traverseOf o id

transposeOf :: Traversal s t [a] a -> s -> [t]
transposeOf o = getZipList #. traverseOf o ZipList

mapAccumLOf :: Traversal s t a b -> (accum -> a -> (b, accum)) -> accum -> s -> (t, accum)
mapAccumLOf o f z s =
  let g a = state $ \ accum -> f accum a
  in runState (traverseOf o g s) z

mapAccumROf :: Traversal s t a b -> (accum -> a -> (b, accum)) -> accum -> s -> (t, accum)
mapAccumROf o = mapAccumLOf (backwards o)
