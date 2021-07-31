module Fresnel.Iso
( -- * Isos
  Iso
, Iso'
  -- * Construction
, iso
, from
  -- * Elimination
, withIso
, under
  -- * Functions
, involuted
, flipped
, curried
, uncurried
  -- * Tuples
, swapped
  -- * Coercion
, coerced
, coercedTo
, coercedFrom
  -- * Functor
, fmapping
) where

import Data.Coerce (Coercible, coerce)
import Data.Profunctor
import Data.Tuple (swap)
import Fresnel.Getter (view)
import Fresnel.Optic
import Fresnel.Profunctor.Coexp
import Fresnel.Review (review)

-- Isos

type Iso s t a b = forall p . Profunctor p => Optic p s t a b

type Iso' s a = Iso s s a a


-- Construction

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap

from :: Iso s t a b -> Iso b a t s
from o = withIso o (flip iso)


-- Elimination

withIso :: Iso s t a b -> (((s -> a) -> (b -> t) -> r) -> r)
withIso i = withCoexp (i (Coexp id id)) . flip


under :: Iso s t a b -> (t -> s) -> (b -> a)
under i = withIso i (\ f r -> (f .) . (. r))


-- Functions

involuted :: (a -> a) -> Iso' a a
involuted f = iso f f

flipped :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = iso flip flip

curried :: Iso ((a, b) -> c) ((a', b') -> c') (a -> b -> c) (a' -> b' -> c')
curried = iso curry uncurry

uncurried :: Iso (a -> b -> c) (a' -> b' -> c') ((a, b) -> c) ((a', b') -> c')
uncurried = iso uncurry curry


-- Tuples

swapped :: Iso (a, b) (a', b') (b, a) (b', a')
swapped = iso swap swap


-- Coercion

coerced :: (Coercible s a, Coercible t b) => Iso s t a b
coerced = coerce `iso` coerce

-- | Build a bidi coercion, taking a constructor for the type being built both to improve type inference and as documentation.
--
-- For example, given two newtypes @A@ and @B@ wrapping the same type, this expression:
--
-- @
-- 'coercedTo' B <<< 'coercedFrom' A
-- @
--
-- produces a bijection of type @'Iso'' A B@.
coercedTo   :: Coercible t b => (s -> a) -> Iso s t a b
coercedTo   = (`iso` coerce)

-- | Build a bidi coercion, taking a constructor for the type being eliminated both to improve type inference and as documentation.
--
-- For example, given two newtypes @A@ and @B@ wrapping the same type, this expression:
--
-- @
-- 'coercedTo' B <<< 'coercedFrom' A
-- @
--
-- produces a bijection of type @'Iso'' A B@.
coercedFrom :: Coercible s a => (b -> t) -> Iso s t a b
coercedFrom = (coerce `iso`)


-- Functor

fmapping :: (Functor f, Functor g) => Iso s t a b -> Iso (f s) (g t) (f a) (g b)
fmapping a = fmap (view a) `iso` fmap (review a)
