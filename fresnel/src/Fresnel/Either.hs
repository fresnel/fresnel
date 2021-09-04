module Fresnel.Either
( -- * Prisms
  _Left
, _Right
) where

import Fresnel.Prism

-- Prisms

_Left :: Prism (Either a b) (Either a' b) a a'
_Left = prism Left (either Right (Left . Right))

_Right :: Prism (Either a b) (Either a b') b b'
_Right = prism Right (either (Left . Left) Right)
