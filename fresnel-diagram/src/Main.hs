module Main
( main
) where

import Text.Blaze.Svg.Renderer.Pretty
import Text.Blaze.Svg11

main :: IO ()
main = putStrLn (renderSvg (svg (do
  style (toMarkup "@import url(https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css);"))))
