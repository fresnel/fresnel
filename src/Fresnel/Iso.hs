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
  -- * Contravariant
, contramapping
  -- * Bifunctor
, bimapping
, firsting
, seconding
  -- * Profunctor
, dimapping
, lmapping
, rmapping
  -- * (Co-)representable
, protabulated
, cotabulated
) where

import Data.Bifunctor
import Data.Coerce (Coercible, coerce)
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Profunctor.Rep hiding (cotabulated)
import Data.Profunctor.Sieve
import Data.Tuple (swap)
import Fresnel.Getter (getting, view)
import Fresnel.Optic
import Fresnel.Profunctor.Coexp
import Fresnel.Review (review, reviewing)

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
fmapping o = withIso o $ \ sa bt -> iso (fmap sa) (fmap bt)


-- Contravariant

contramapping :: (Contravariant f, Contravariant g) => Iso s t a b -> Iso (f a) (g b) (f s) (g t)
contramapping o = withIso o $ \ sa bt -> iso (contramap sa) (contramap bt)


-- Bifunctor

bimapping :: (Bifunctor p, Bifunctor q) => Iso s t a b -> Iso s' t' a' b' -> Iso (p s s') (q t t') (p a a') (q b b')
bimapping a b = withIso a $ \ lsa lbt -> withIso b $ \ rsa rbt -> iso (bimap lsa rsa) (bimap lbt rbt)

firsting :: (Bifunctor p, Bifunctor q) => Iso s t a b -> Iso (p s x) (q t y) (p a x) (q b y)
firsting a = withIso a $ \ sa bt -> iso (first sa) (first bt)

seconding :: (Bifunctor p, Bifunctor q) => Iso s t a b -> Iso (p x s) (q y t) (p x a) (q y b)
seconding b = withIso b $ \ sa bt -> iso (second sa) (second bt)


-- Profunctor

dimapping :: (Profunctor p, Profunctor q) => Iso s t a b -> Iso s' t' a' b' -> Iso (p a s') (q b t') (p s a') (q t b')
dimapping a b = dimap (view (getting a)) (view (getting b)) `iso` dimap (review (reviewing a)) (review (reviewing b))

lmapping :: (Profunctor p, Profunctor q) => Iso s t a b -> Iso (p a x) (q b y) (p s x) (q t y)
lmapping a = lmap (view (getting a)) `iso` lmap (review (reviewing a))

rmapping :: (Profunctor p, Profunctor q) => Iso s t a b -> Iso (p x s) (q y t) (p x a) (q y b)
rmapping b = rmap (view (getting b)) `iso` rmap (review (reviewing b))


-- (Co-)representable (profunctorial)

protabulated :: (Representable p, Representable q) => Iso (a -> Rep p b) (a' -> Rep q b') (a `p` b) (a' `q` b')
protabulated = tabulate `iso` sieve

cotabulated :: (Corepresentable p, Corepresentable q) => Iso (Corep p a -> b) (Corep q a' -> b') (a `p` b) (a' `q` b')
cotabulated = cotabulate `iso` cosieve
