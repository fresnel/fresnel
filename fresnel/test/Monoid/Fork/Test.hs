{-# LANGUAGE TemplateHaskell #-}
module Monoid.Fork.Test
( tests
) where

import Data.Foldable (toList)
import Data.Ratio
import Fresnel.Monoid.Fork (Fork(runFork), singleton)
import Test.Group
import Test.QuickCheck as QC hiding (total)

prop_semigroup_assoc :: (Eq a, Show a) => ArbFork a -> ArbFork a -> ArbFork a -> Property
prop_semigroup_assoc (ArbFork a) (ArbFork b) (ArbFork c) =
  label (summarize a) . label (summarize b) . label (summarize c) $
  (toList (a <> (b <> c)) === toList ((a <> b) <> c))

prop_fail :: (Eq a, Show a) => a -> a -> Property
prop_fail a b = a === b

prop_monoid_identity :: (Eq a, Show a) => ArbFork a -> Property
prop_monoid_identity (ArbFork a) = label (summarize a) $ toList (mempty <> a) === toList a .&&. toList (a <> mempty) === toList a


newtype ArbFork a = ArbFork (Fork a)
  deriving (Show)

instance Arbitrary a => Arbitrary (ArbFork a) where
  arbitrary = ArbFork <$> sized go where
    go 0 = pure mempty
    go i = oneof
      [ chooseInt (0, i) >>= \ j -> (<>) <$> go (i - j) <*> go j
      , singleton <$> arbitrary
      , pure mempty
      ]


summarize :: Fork a -> String
summarize r
  | total' == nils     = "nil"
  | ratio nils > 0.4   = "nil-heavy"
  | total' == leaves   = "leaf"
  | ratio leaves > 0.4 = "leaf-heavy"
  | ratio forks > 0.4  = "fork-heavy"
  | otherwise          = "fork"
  where
  (total', Counts forks leaves nils) = (,) . total <*> id $ runFork r (\ l r -> fork <> l <> r) (const leaf) nil
  ratio a = realToFrac  (a % total') :: Double

data Counts = Counts
  { forks  :: {-# UNPACK #-} !Int
  , leaves :: {-# UNPACK #-} !Int
  , nils   :: {-# UNPACK #-} !Int
  }

instance Semigroup Counts where
  c1 <> c2 = Counts (forks c1 + forks c2) (leaves c1 + leaves c2) (nils c1 + nils c2)

fork :: Counts
fork = Counts 1 0 0

leaf :: Counts
leaf = Counts 0 1 0

nil :: Counts
nil = Counts 0 0 1

total :: Counts -> Int
total (Counts f l n) = f + l + n


pure []

tests :: Group
tests = $deriveGroup
