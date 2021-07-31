{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( validIso
, validIso1
, invalidIso
, withRoundtrips
, tripL
, tripR
, test
) where

import Fresnel.Getter
import Fresnel.Iso
import Fresnel.Review
import Test.QuickCheck

validIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
validIso o s a = (tripR o a === a) .&&. (tripL o s === s)

validIso1 :: (Eq b, Show b) => Iso' (a -> b) b -> a -> b -> Property
validIso1 o a b = (tripR o b === b) .&&. (tripL o (const b) a === b)

invalidIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
invalidIso o s a = (tripR o a =/= a) .||. (tripL o s =/= s)

withRoundtrips :: Iso' s a -> (((s -> s) -> (a -> a) -> r) -> r)
withRoundtrips o k = withIso o (\ f g -> k (g . f) (f . g))

tripL :: Iso' s a -> (s -> s)
tripL o = withRoundtrips o const

tripR :: Iso' s a -> (a -> a)
tripR o = withRoundtrips o (const id)


prop_view_elimination f g x = view (iso (applyFun f) (applyFun g)) x === applyFun f x

prop_review_elimination f g x = review (iso (applyFun f) (applyFun g)) x === applyFun g x


prop_constant_validity c = validIso1 (constant c)


prop_involuted_validity = validIso (involuted not)

prop_involuted_invalidity = invalidIso (involuted (+ (1 :: Integer)))


pure []

test :: IO Bool
test = $quickCheckAll
