{-# LANGUAGE TemplateHaskell #-}
module Review.Test
( tests
) where
import Fresnel.Review
import Test.Group
import Test.QuickCheck

prop_review_unto_involution (Fn f) x = review (unto f) x === f x


pure []

tests :: Entry
tests = $deriveGroup
