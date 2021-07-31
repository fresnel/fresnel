{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( test
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
