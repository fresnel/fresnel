module Fresnel.Profunctor.Market
( Market(..)
) where

data Market s t a b = Market { inj :: b -> t, prj :: s -> Either t a }
