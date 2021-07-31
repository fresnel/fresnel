{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Fresnel.Profunctor.Recall
( -- * Recall profunctor
  Recall(..)
) where

import Data.Bifunctor
import Data.Functor.Const
import Data.Profunctor
import Data.Profunctor.Rep as Pro
import Data.Profunctor.Sieve

-- * Recall profunctor

-- | @'Recall' e@ is dual to @'Forget' r@: it ignores the argument parameter, substituting in one of its own.
newtype Recall e a b = Recall { runRecall :: e -> b }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Bifunctor (Recall e) where
  bimap _ g = Recall . fmap g . runRecall
  second = fmap

instance Profunctor (Recall e) where
  dimap _ g = Recall . fmap g . runRecall
  rmap = fmap

instance Choice (Recall e) where
  left'  = Recall . fmap Left  . runRecall
  right' = Recall . fmap Right . runRecall

instance Closed (Recall e) where
  closed = Recall . fmap const . runRecall

instance Costrong (Recall e) where
  unfirst  = Recall . fmap fst . runRecall
  unsecond = Recall . fmap snd . runRecall

instance Sieve (Recall e) ((->) e) where
  sieve = const . runRecall

instance Cosieve (Recall e) (Const e) where
  cosieve = lmap getConst . runRecall

instance Pro.Corepresentable (Recall e) where
  type Corep (Recall e) = Const e

  cotabulate = Recall . lmap Const
