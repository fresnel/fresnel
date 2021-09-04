module Fresnel.Maybe
( -- * Prisms
  _Just
, _Nothing
) where

import Fresnel.Prism

-- Prisms

_Just :: Prism (Maybe a) (Maybe a') a a'
_Just = prism Just (maybe (Left Nothing) Right)

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) (maybe (Just ()) (const Nothing))
