{-# LANGUAGE TemplateHaskell #-}
module Review.Test
( tests
) where
import Fresnel.Review
import Test.Group
import Test.QuickCheck

prop_review_unto_involution f x = review (unto (applyFun f)) x === applyFun f x


pure []

tests :: Entry
tests = $deriveGroup
