{-# LANGUAGE TemplateHaskell #-}
module Profunctor.Coexp.Test
( tests
) where

import Fresnel.Profunctor.Coexp
import Test.Group
import Test.QuickCheck

prop_semigroup_assoc :: (Eq b, Eq a, Show b, Show a) => ArbCoexp b a a b -> ArbCoexp b a a b -> ArbCoexp b a a b -> b -> a -> Property
prop_semigroup_assoc a b c x y =
  appCoexp (toCoexp a <> (toCoexp b <> toCoexp c)) x y === appCoexp ((toCoexp a <> toCoexp b) <> toCoexp c) x y

prop_monoid_identity :: (Eq b, Show b) => ArbCoexp b a a b -> b -> Property
prop_monoid_identity a x = recall (mempty <> toCoexp a) x === recall (toCoexp a) x .&&. recall (toCoexp a <> mempty) x === recall (toCoexp a) x


data ArbCoexp e r a b = ArbCoexp (Fun e b) (Fun a r)
  deriving (Show)

instance (Function e, Function a, CoArbitrary e, CoArbitrary a, Arbitrary b, Arbitrary r) => Arbitrary (ArbCoexp e r a b) where
  arbitrary = ArbCoexp <$> arbitrary <*> arbitrary

toCoexp :: ArbCoexp e r a b -> Coexp e r a b
toCoexp (ArbCoexp eb ar) = coexp (applyFun eb) (applyFun ar)

appCoexp :: Coexp e r a b -> e -> a -> (b, r)
appCoexp c e a = withCoexp c $ \ recall forget -> (recall e, forget a)


pure []

tests :: Group
tests = $mkGroup
