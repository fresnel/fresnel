{-# LANGUAGE RankNTypes #-}
module Fresnel.Traversal
( -- * Traversals
  Traversal
, Traversal'
, IsTraversal
  -- * Construction
, traversal
, traversed
, backwards
, both
, beside
, ignored
  -- * Elimination
, traverseOf
, forOf
, sequenceOf
, transposeOf
, mapAccumLOf
, mapAccumROf
, scanl1Of
, scanr1Of
) where

import Control.Applicative (ZipList(..))
import Control.Monad.Trans.State
import Data.Bitraversable (Bitraversable(..))
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

traversal :: (forall f . Applicative f => (a -> f b) -> (s -> f t)) -> Traversal s t a b
traversal = wander

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = traversal traverse

backwards :: Traversal s t a b -> Traversal s t a b
backwards o = traversal (\ f -> forwards #. traverseOf o (Backwards #. f))

both :: Bitraversable r => Traversal (r a a) (r b b) a b
both = traversal (\ f -> bitraverse f f)

beside :: Bitraversable r => Traversal s1 t1 a b -> Traversal s2 t2 a b -> Traversal (r s1 s2) (r t1 t2) a b
beside l r = traversal (\ f -> bitraverse (traverseOf l f) (traverseOf r f))

-- | The trivially empty @'Traversal'@.
--
-- @
-- 'traverseOf' 'ignored' f = pure
-- @
ignored :: Traversal' s a
ignored = traversal (const pure)


-- Elimination

traverseOf :: Applicative f => Traversal s t a b -> ((a -> f b) -> (s -> f t))
traverseOf o = runStar #. o . Star

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

scanl1Of :: Traversal s t a a -> (a -> a -> a) -> s -> t
scanl1Of o f =
  let step Nothing  a = (a, Just a)
      step (Just s) a = let r = f s a in (r, Just r)
  in fst . mapAccumLOf o step Nothing

scanr1Of :: Traversal s t a a -> (a -> a -> a) -> s -> t
scanr1Of o f =
  let step Nothing  a = (a, Just a)
      step (Just s) a = let r = f s a in (r, Just r)
  in fst . mapAccumROf o step Nothing
