module Fresnel.Profunctor.Recall
( -- * Recall profunctor
  Recall(..)
) where

-- * Recall profunctor

newtype Recall e a b = Recall { runRecall :: e -> b }
