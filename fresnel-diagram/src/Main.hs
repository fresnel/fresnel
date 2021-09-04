{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import Text.Blaze.Svg.Renderer.Pretty
import Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes as A

main :: IO ()
main = putStrLn . renderSvg $ svg ! version "1.1" ! xmlns "http://www.w3.org/2000/svg" ! viewbox "-575 -50 1300 650" $ do
  S.style (toMarkup ("@import url(https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css);" :: String))

xmlns = customAttribute "xmlns"
