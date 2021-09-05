{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main
( main
) where

import           Data.Foldable (for_)
import           Data.Function (fix)
import           System.Environment (getArgs)
import           Text.Blaze.Svg.Renderer.Pretty
import           Text.Blaze.Svg11 as S hiding (z)
import qualified Text.Blaze.Svg11.Attributes as A

main :: IO ()
main = do
  let rendered = renderSvg $ svg ! A.version "1.1" ! xmlns "http://www.w3.org/2000/svg" ! A.viewbox "-575 -50 1300 650" $ do
        S.style (toMarkup ("@import url(https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css);" :: String))
        for_ graph renderVertex
  getArgs >>= \case
    []     -> putStrLn rendered
    path:_ -> writeFile path rendered

renderVertex :: Vertex -> Svg
renderVertex Vertex{ kind, name = n, coords, outEdges } = do
  let P2 h v = project coords
  g ! A.id_ (stringValue n) ! A.class_ (stringValue ("vertex " <> show kind)) ! A.transform (translate h v) $ do
    for_ outEdges $ \ dest -> S.path ! A.id_ (stringValue (n <> "-" <> name dest))
    circle ! A.r "2.5"
    text_ (toMarkup n)
  where
  project (P3 x y z) = P2 (x * negate 200 + y * 200) ((x + y) * 100 - z * 100)

xmlns = customAttribute "xmlns"


data Vertex = Vertex
  { kind     :: VertexKind
  , name     :: String
  , coords   :: P3 Int
  , outEdges :: [Vertex]
  }

data VertexKind
  = Optic
  | Class
  deriving (Eq, Ord, Show)

data P3 a = P3 a a a
  deriving (Functor)

data P2 a = P2 a a
  deriving (Functor)

graph :: [Vertex]
graph = fix $ \case
  ~[iso, lens, getter, prism, review, optional, affineFold, traversal, fold, setter, _profunctor, strong, cochoice, _bicontravariant, choice, costrong, _bifunctor, closed, traversing, mapping] ->
    [ Vertex Optic "Iso" (P3 0 0 0) [lens, prism]
    , Vertex Optic "Lens" (P3 1 0 0) [optional, getter]
    , Vertex Optic "Getter" (P3 2 0 0) [affineFold]
    , Vertex Optic "Prism" (P3 0 1 0) [optional, review]
    , Vertex Optic "Review" (P3 0 2 0) []
    , Vertex Optic "Optional" (P3 1 1 0) [affineFold, traversal]
    , Vertex Optic "AffineFold" (P3 2 1 0) [fold]
    , Vertex Optic "Traversal" (P3 1 2 0) [fold, setter]
    , Vertex Optic "Fold" (P3 2 2 0) []
    , Vertex Optic "Setter" (P3 1 3 0) []
    , Vertex Class "Profunctor" (P3 0 0 1) [iso, strong, choice, cochoice, costrong, closed]
    , Vertex Class "Strong" (P3 1 0 1) [lens, traversing]
    , Vertex Class "Cochoice" (P3 2 0 1) [getter]
    , Vertex Class "Bicontravariant" (P3 2 0 2) [getter]
    , Vertex Class "Choice" (P3 0 1 1) [prism, traversing]
    , Vertex Class "Costrong" (P3 0 2 1) [review]
    , Vertex Class "Bifunctor" (P3 0 2 2) [review]
    , Vertex Class "Closed" (P3 0 3 1) [mapping]
    , Vertex Class "Traversing" (P3 1 2 1) [traversal, mapping]
    , Vertex Class "Mapping" (P3 1 3 1) [setter]
    ]
  _ -> error "not enough vertices, or too many. either way, it’s bad."
