{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( validIso
, validIso1
, invalidIso
, withRoundtrips
, test
) where

import Fresnel.Getter
import Fresnel.Iso
import Fresnel.Review
import Test.QuickCheck

validIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
validIso o s a = withRoundtrips o $ \ ss aa -> (aa a === a) .&&. (ss s === s)

validIso1 :: (Eq a, Show a) => Iso' (s -> a) a -> (s -> a) -> s -> a -> Property
validIso1 o f s a = withRoundtrips o $ \ sasa aa -> (aa a === a) .&&. (sasa f s === f s)

invalidIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
invalidIso o s a = withRoundtrips o $ \ ss aa -> (aa a =/= a) .||. (ss s =/= s)

withRoundtrips :: Iso' s a -> (((s -> s) -> (a -> a) -> r) -> r)
withRoundtrips o k = withIso o (\ f g -> k (g . f) (f . g))


prop_view_elimination f g x = view (iso (applyFun f) (applyFun g)) x === applyFun f x

prop_review_elimination f g x = review (iso (applyFun f) (applyFun g)) x === applyFun g x


prop_constant_validity c s = validIso1 (constant c) (const s) s


prop_involuted_validity = validIso (involuted not)

prop_involuted_invalidity = invalidIso (involuted (+ (1 :: Integer)))


pure []

test :: IO Bool
test = $quickCheckAll
