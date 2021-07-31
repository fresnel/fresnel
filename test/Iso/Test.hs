{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( test
, validIso
) where

import Fresnel.Getter
import Fresnel.Iso
import Fresnel.Review
import Test.QuickCheck

prop_view_elimination f g x = view (iso (applyFun f) (applyFun g)) x === applyFun f x

prop_review_elimination f g x = review (iso (applyFun f) (applyFun g)) x === applyFun g x


pure []

test :: IO Bool
test = $quickCheckAll


validIso :: (Eq a, Show a, Eq s, Show s) => Iso' s a -> s -> a -> Property
validIso o s a = ((view o . review o) a === a) .&&. ((review o . view o) s === s)
