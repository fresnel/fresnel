{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold
( -- * Folds
  Fold
, IsFold
  -- * Construction
, folded
, unfolded
, folding
, foldring
, foldMapping
, foldMap1ing
, ignored
, backwards
, iterated
, filtered
, repeated
, replicated
, cycled
, takingWhile
  -- * Elimination
, has
, hasn't
, foldMapOf
, foldMapByOf
, foldrOf
, foldlOf'
, foldOf
, foldByOf
, sequenceOf_
, traverseOf_
, forOf_
, toListOf
, anyOf
, allOf
, noneOf
, andOf
, orOf
, productOf
, sumOf
, altOf
, asumOf
, concatOf
, concatMapOf
, elemOf
, notElemOf
, lengthOf
, nullOf
, notNullOf
, firstOf
, lastOf
, minimumOf
, minimumByOf
, maximumOf
, maximumByOf
, findOf
, findMOf
, lookupOf
, previews
, preview
, (^?)
, Failover(..)
, Union(..)
) where

import Control.Applicative (Alternative(..))
import Control.Applicative.Backwards
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Functor.Contravariant
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Unsafe ((#.), (.#))
import Fresnel.Bifunctor.Contravariant
import Fresnel.Fold1.Internal (IsFold1)
import Fresnel.Functor.Traversed
import Fresnel.Monoid.Cons as Cons
import Fresnel.Monoid.Fork as Fork
import Fresnel.Monoid.Snoc as Snoc
import Fresnel.Optic
import Fresnel.OptionalFold.Internal (IsOptionalFold)
import Fresnel.Traversal (IsTraversal, ignored, traversal)

-- Folds

type Fold s a = forall p . IsFold p => Optic' p s a

class (IsOptionalFold p, IsFold1 p, IsTraversal p) => IsFold p

instance Monoid r => IsFold (Forget r)
instance (Applicative f, Traversable f, Contravariant f) => IsFold (Star f)


-- Construction

folded :: Foldable f => Fold (f a) a
folded = rphantom . traversal traverse_

unfolded :: (s -> Maybe (a, s)) -> Fold s a
unfolded coalg = rphantom . traversal (\ f -> let loop = maybe (pure ()) (\ (a, s) -> f a *> loop s) . coalg in loop)

folding :: Foldable f => (s -> f a) -> Fold s a
folding f = contrabimap f (const ()) . traversal traverse_

foldring :: (forall f . Applicative f => (a -> f u -> f u) -> f v -> s -> f w) -> Fold s a
foldring fr = rphantom . traversal (\ f -> fr (\ a -> (f a *>)) (pure v)) where
  v = error "foldring: value used"

-- | Make a 'Fold' by lifting a 'foldMap'-like function.
--
-- @
-- 'foldMapping' 'foldMap' = 'folded'
-- @
foldMapping :: (forall m . Monoid m => (a -> m) -> (s -> m)) -> Fold s a
foldMapping fm = rphantom . traversal (\ f -> getAp . fm (Ap . void . f))

-- | Make a 'Fold' by lifting a 'foldMap1'-like function.
foldMap1ing :: (forall m . Semigroup m => (a -> m) -> (s -> m)) -> Fold s a
foldMap1ing fm = rphantom . traversal (\ f -> getAp . fm (Ap . void . f))

backwards :: Fold s a -> Fold s a
backwards o = rphantom . traversal (\ f -> forwards . traverseOf_ o (Backwards #. f))

iterated :: (a -> a) -> Fold a a
iterated f = rphantom . traversal (\ g -> let loop a = g a *> loop (f a) in loop)

filtered :: (a -> Bool) -> Fold a a
filtered p = folding (\ a -> if p a then Just a else Nothing)

-- | An infinite fold repeatedly producing its input.
--
-- @
-- 'toListOf' 'repeated' a = 'repeat' a
-- @
repeated :: Fold a a
repeated = rphantom . traversal (\ f a -> let loop = f a *> loop in loop)

-- | A fold producing its input a finite number of times.
--
-- @
-- 'toListOf' ('replicated' n) a = 'replicate' n a
-- @
replicated :: Int -> Fold a a
replicated n0 = rphantom . traversal (\ f -> let loop n a = if n <= 0 then pure () else f a *> loop (n - 1) a in loop n0)

cycled :: Fold s a -> Fold s a
cycled f = foldring (\ cons _ s -> let loop = foldrOf f cons loop s in loop)

takingWhile :: (a -> Bool) -> Fold s a -> Fold s a
takingWhile p f = foldring (\ cons nil -> foldrOf f (\ a as -> if p a then cons a as else nil) nil)


-- Elimination

has :: Fold s a -> (s -> Bool)
has = notNullOf

hasn't :: Fold s a -> (s -> Bool)
hasn't = nullOf


foldMapOf :: Monoid m => Fold s a -> ((a -> m) -> (s -> m))
foldMapOf o = runForget #. o .# Forget

foldMapByOf :: Fold s a -> ((r -> r -> r) -> r -> (a -> r) -> (s -> r))
foldMapByOf o fork nil leaf s = runFork (runForget (o (Forget Fork.singleton)) s) fork leaf nil

foldrOf :: Fold s a -> ((a -> r -> r) -> r -> s -> r)
foldrOf o cons nil s = runCons (runForget (o (Forget Cons.singleton)) s) cons nil

foldlOf' :: Fold s a -> ((r -> a -> r) -> r -> s -> r)
foldlOf' o snoc nil s = runSnoc (runForget (o (Forget Snoc.singleton)) s) snoc nil

foldOf :: Monoid a => Fold s a -> (s -> a)
foldOf o = foldMapOf o id

foldByOf :: Fold s a -> ((a -> a -> a) -> a -> (s -> a))
foldByOf o fork nil s = runFork (runForget (o (Forget Fork.singleton)) s) fork id nil

sequenceOf_ :: Applicative f => Fold s (f a) -> (s -> f ())
sequenceOf_ o = runTraversed . foldMapOf o Traversed

traverseOf_ :: Applicative f => Fold s a -> ((a -> f r) -> (s -> f ()))
traverseOf_ o f = runTraversed . foldMapOf o (Traversed #. f)

forOf_ :: Applicative f => Fold s a -> (s -> (a -> f r) -> f ())
forOf_ o = flip (traverseOf_ o)

toListOf :: Fold s a -> s -> [a]
toListOf o = foldrOf o (:) []

anyOf :: Fold s a -> (a -> Bool) -> (s -> Bool)
anyOf o = foldMapByOf o (||) False

allOf :: Fold s a -> (a -> Bool) -> (s -> Bool)
allOf o = foldMapByOf o (&&) True

noneOf :: Fold s a -> (a -> Bool) -> (s -> Bool)
noneOf o p = anyOf o (not . p)

andOf :: Fold s Bool -> (s -> Bool)
andOf o = getAll #. foldMapOf o All

orOf :: Fold s Bool -> (s -> Bool)
orOf o = getAny #. foldMapOf o Any

productOf :: Num a => Fold s a -> (s -> a)
productOf o = getProduct #. foldMapOf o Product

sumOf :: Num a => Fold s a -> (s -> a)
sumOf o = getSum #. foldMapOf o Sum

altOf :: Alternative f => Fold s a -> (s -> f a)
altOf o = getAlt #. foldMapOf o (Alt #. pure)

asumOf :: Alternative f => Fold s (f a) -> (s -> f a)
asumOf o = getAlt #. foldMapOf o Alt

concatOf :: Fold s [a] -> (s -> [a])
concatOf = foldOf

concatMapOf :: Fold s a -> ((a -> [r]) -> (s -> [r]))
concatMapOf = foldMapOf

elemOf :: Eq a => Fold s a -> a -> s -> Bool
elemOf o = anyOf o . (==)

notElemOf :: Eq a => Fold s a -> a -> s -> Bool
notElemOf o = noneOf o . (==)

lengthOf :: Fold s a -> (s -> Int)
lengthOf o = foldrOf o (const (+ 1)) 0

nullOf :: Fold s a -> (s -> Bool)
nullOf o = foldrOf o (\ _ _ -> False) True

notNullOf :: Fold s a -> (s -> Bool)
notNullOf o = foldrOf o (\ _ _ -> True) False

firstOf :: Fold s a -> (s -> Maybe a)
firstOf o = foldrOf o (\ a _ -> Just a) Nothing

lastOf :: Fold s a -> (s -> Maybe a)
lastOf o = getLast #. foldMapOf o (Last #. Just)

minimumOf :: Ord a => Fold s a -> (s -> Maybe a)
minimumOf o = minimumByOf o compare

minimumByOf :: Fold s a -> (a -> a -> Ordering) -> (s -> Maybe a)
minimumByOf o cmp = foldlOf' o (\ a b -> Just (case a of
  Nothing -> b
  Just a
    | GT <- cmp a b -> b
    | otherwise     -> a)) Nothing

maximumOf :: Ord a => Fold s a -> (s -> Maybe a)
maximumOf o = maximumByOf o compare

maximumByOf :: Fold s a -> (a -> a -> Ordering) -> (s -> Maybe a)
maximumByOf o cmp = foldlOf' o (\ a b -> Just (case a of
  Nothing -> b
  Just a
    | LT <- cmp a b -> b
    | otherwise     -> a)) Nothing

findOf :: Fold s a -> (a -> Bool) -> (s -> Maybe a)
findOf o p = foldrOf o (\ a as -> if p a then Just a else as) Nothing

findMOf :: Monad m => Fold s a -> (a -> m Bool) -> (s -> m (Maybe a))
findMOf o p = foldrOf o (\ a as -> do
  c <- p a
  if c then
    return (Just a)
  else
    as) (return Nothing)

lookupOf :: Eq k => Fold s (k, v) -> (k -> s -> Maybe v)
lookupOf o k = fmap snd . findOf o ((== k) . fst)


previews :: Fold s a -> (a -> r) -> (s -> Maybe r)
previews o f = getFirst #. foldMapOf o (First #. Just . f)

preview :: Fold s a -> s -> Maybe a
preview o = previews o id

(^?) :: s -> Fold s a -> Maybe a
s ^? o = preview o s

infixl 8 ^?


newtype Failover s a = Failover { getFailover :: Fold s a }

instance Semigroup (Failover s a) where
  Failover a1 <> Failover a2 = Failover (folding (\ s -> Cons (\ cons nil -> maybe (foldrOf a2 cons nil s) (uncurry cons) (foldrOf a1 (\ a -> Just . (,) a . maybe nil (uncurry cons)) Nothing s))))

instance Monoid (Failover s a) where
  mempty = Failover ignored


newtype Union s a = Union { getUnion :: Fold s a }

instance Semigroup (Union s a) where
  Union a1 <> Union a2 = Union (rphantom . traversal (\ f s -> traverseOf_ a1 f s *> traverseOf_ a2 f s) . rphantom)

instance Monoid (Union s a) where
  mempty = Union ignored
