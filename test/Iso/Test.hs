{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( validIso
, validIso1
, invalidIso
, tripL
, tripR
, test
) where

import Fresnel.Getter
import Fresnel.Iso
import Fresnel.Review
import Test.QuickCheck

validIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
validIso o s a = ((view o . review o) a === a) .&&. ((review o . view o) s === s)

validIso1 :: (Eq b, Show b) => Iso' (a -> b) b -> a -> b -> Property
validIso1 o a b = (view o (review o b) === b) .&&. (review o (view o (const b)) a === b)

invalidIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
invalidIso o s a = ((view o . review o) a =/= a) .||. ((review o . view o) s =/= s)

tripL :: Iso' s a -> (s -> s)
tripL o = review o . view o

tripR :: Iso' s a -> (a -> a)
tripR o = view o . review o


prop_view_elimination f g x = view (iso (applyFun f) (applyFun g)) x === applyFun f x

prop_review_elimination f g x = review (iso (applyFun f) (applyFun g)) x === applyFun g x


prop_constant_validity c = validIso1 (constant c)


prop_involuted_validity = validIso (involuted not)

prop_involuted_invalidity = invalidIso (involuted (+ (1 :: Integer)))


pure []

test :: IO Bool
test = $quickCheckAll
