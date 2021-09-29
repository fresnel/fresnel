{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( validIso
, invalidIso
, withRoundtrips
, tests
) where

import Fresnel.Getter
import Fresnel.Iso
import Fresnel.Review
import Test.Group
import Test.QuickCheck as QC

validIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
validIso o s a = withRoundtrips o $ \ ss aa -> ss s === s .&&. aa a === a

invalidIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
invalidIso o s a = withRoundtrips o $ \ ss aa -> ss s =/= s .||. aa a =/= a

withRoundtrips :: Iso' s a -> (((s -> s) -> (a -> a) -> r) -> r)
withRoundtrips o k = withIso o (\ f g -> k (g . f) (f . g))


prop_view_elimination f g x = view (iso (applyFun f) (applyFun g)) x === applyFun f x

prop_review_elimination f g x = review (iso (applyFun f) (applyFun g)) x === applyFun g x

prop_fail :: (Eq a, Show a) => a -> a -> Property
prop_fail a b = a === b

prop_constant_validity c s a = withRoundtrips (constant c) $ \ sasa aa ->
  sasa (const a) s === const a s .&&. aa a === a


prop_involuted_validity = validIso (involuted not)

prop_involuted_invalidity = invalidIso (involuted (+ (1 :: Integer)))


pure []

tests :: Group
tests = $deriveGroup
