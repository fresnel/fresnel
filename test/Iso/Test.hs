{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( validIso
, invalidIso
, test
) where

import Fresnel.Getter
import Fresnel.Iso
import Fresnel.Review
import Test.QuickCheck

validIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
validIso o s a = ((view o . review o) a === a) .&&. ((review o . view o) s === s)

invalidIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
invalidIso o s a = ((view o . review o) a =/= a) .||. ((review o . view o) s =/= s)


prop_view_elimination f g x = view (iso (applyFun f) (applyFun g)) x === applyFun f x

prop_review_elimination f g x = review (iso (applyFun f) (applyFun g)) x === applyFun g x


prop_involuted_validity = validIso (involuted not)

prop_involuted_invalidity = invalidIso (involuted (+ (1 :: Integer)))


pure []

test :: IO Bool
test = $quickCheckAll