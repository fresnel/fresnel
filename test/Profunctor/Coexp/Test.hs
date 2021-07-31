{-# LANGUAGE TemplateHaskell #-}
module Profunctor.Coexp.Test
( test
) where

import Fresnel.Profunctor.Coexp
import Test.QuickCheck

prop_semigroup_assoc :: (Eq b, Eq a, Show b, Show a) => ArbCoexp b a a b -> ArbCoexp b a a b -> ArbCoexp b a a b -> b -> a -> Property
prop_semigroup_assoc a b c x y =
  appCoexp (coexp a <> (coexp b <> coexp c)) x y === appCoexp ((coexp a <> coexp b) <> coexp c) x y

prop_monoid_left_identity :: (Eq b, Show b) => ArbCoexp b a a b -> b -> Property
prop_monoid_left_identity a x = recall (mempty <> coexp a) x === recall (coexp a) x


data ArbCoexp e r a b = ArbCoexp (Fun e b) (Fun a r)
  deriving (Show)

coexp :: ArbCoexp e r a b -> Coexp e r a b
coexp (ArbCoexp eb ar) = Coexp (applyFun eb) (applyFun ar)

appCoexp :: Coexp e r a b -> e -> a -> (b, r)
appCoexp c e a = (recall c e, forget c a)


instance (Function e, Function a, CoArbitrary e, CoArbitrary a, Arbitrary b, Arbitrary r) => Arbitrary (ArbCoexp e r a b) where
  arbitrary = ArbCoexp <$> arbitrary <*> arbitrary


pure []

test :: IO Bool
test = $quickCheckAll
