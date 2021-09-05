{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main
( main
, P2(..)
, P3(..)
) where

import           Control.Applicative (liftA2)
import           Control.Monad (join)
import           Data.Foldable (for_)
import           Data.Function (fix)
import           System.Environment (getArgs)
import           Text.Blaze.Svg.Renderer.Pretty
import           Text.Blaze.Svg11 as S hiding (z)
import qualified Text.Blaze.Svg11.Attributes as A

main :: IO ()
main = do
  let rendered = renderSvg $ svg ! A.version "1.1" ! xmlns "http://www.w3.org/2000/svg" ! A.viewbox "-575 -50 1300 650" $ do
        S.style (toMarkup ("@import url(./optics.css);" :: String))
        for_ graph renderVertex
  getArgs >>= \case
    []     -> putStrLn rendered
    path:_ -> writeFile path rendered

renderVertex :: Vertex -> Svg
renderVertex Vertex{ kind, name, coords = coords@P3{ x, y }, outEdges } = do
  let p = scale (project coords)
  g ! A.id_ (stringValue name) ! A.class_ (stringValue ("vertex " <> show kind)) ! A.transform (uncurryP2 translate p) $ do
    for_ outEdges $ \ Vertex{ name = dname, coords = dcoords@P3{ x = dx, y = dy} } ->
      S.path ! A.id_ (stringValue (name <> "-" <> dname)) ! A.class_ (stringValue (show kind)) ! A.d (mkPath (do
        let δ = scale (project (dcoords - coords))
            sδ = signum δ
        if x == dx && y == dy then do
          uncurryP2 mr (sδ * voffset)
          uncurryP2 lr (δ - voffset * 2)
        else do
          uncurryP2 mr (sδ * hoffset)
          uncurryP2 lr (δ - sδ * hoffset * 2)
          uncurryP2 mr (sδ * P2 (-6) (-8))
          uncurryP2 lr (sδ * P2 6 8)
          uncurryP2 lr (sδ * P2 (-10) 0)
        ))
    circle ! A.r "2.5"
    text_ (toMarkup name)
  where
  hoffset = P2 10 5
  voffset = P2 0 10
  project (P3 x y z) = P2 (negate x + y) (x + y - z)
  scale (P2 x y) = P2 (x * 200) (y * 100)

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

data P3 a = P3 { x :: a, y :: a, z :: a }
  deriving (Functor)

instance Applicative P3 where
  pure = join (join P3)
  P3 f1 f2 f3 <*> P3 a1 a2 a3 = P3 (f1 a1) (f2 a2) (f3 a3)

instance Num a => Num (P3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

data P2 a = P2 { x :: a, y :: a }
  deriving (Functor)

instance Applicative P2 where
  pure = join P2
  P2 f1 f2 <*> P2 a1 a2 = P2 (f1 a1) (f2 a2)

instance Num a => Num (P2 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

uncurryP2 :: (a -> a -> b) -> (P2 a -> b)
uncurryP2 f (P2 x y) = f x y

graph :: Diagram Vertex
graph = fix $ \ ~Diagram{ iso, lens, getter, prism, review, optional, affineFold, traversal, fold, setter, strong, cochoice, choice, costrong, closed, traversing, mapping } -> Diagram
      { iso             = Vertex Optic "Iso" (P3 0 0 0) [lens, prism]
      , lens            = Vertex Optic "Lens" (P3 1 0 0) [optional, getter]
      , getter          = Vertex Optic "Getter" (P3 2 0 0) [affineFold]
      , prism           = Vertex Optic "Prism" (P3 0 1 0) [optional, review]
      , review          = Vertex Optic "Review" (P3 0 2 0) []
      , optional        = Vertex Optic "Optional" (P3 1 1 0) [affineFold, traversal]
      , affineFold      = Vertex Optic "AffineFold" (P3 2 1 0) [fold]
      , traversal       = Vertex Optic "Traversal" (P3 1 2 0) [fold, setter]
      , fold            = Vertex Optic "Fold" (P3 2 2 0) []
      , setter          = Vertex Optic "Setter" (P3 1 3 0) []
      , profunctor      = Vertex Class "Profunctor" (P3 0 0 1) [iso, strong, choice, cochoice, costrong, closed]
      , strong          = Vertex Class "Strong" (P3 1 0 1) [lens, traversing]
      , cochoice        = Vertex Class "Cochoice" (P3 2 0 1) [getter]
      , bicontravariant = Vertex Class "Bicontravariant" (P3 2 0 2) [getter]
      , choice          = Vertex Class "Choice" (P3 0 1 1) [prism, traversing]
      , costrong        = Vertex Class "Costrong" (P3 0 2 1) [review]
      , bifunctor       = Vertex Class "Bifunctor" (P3 0 2 2) [review]
      , closed          = Vertex Class "Closed" (P3 0 3 1) [mapping]
      , traversing      = Vertex Class "Traversing" (P3 1 2 1) [traversal, mapping]
      , mapping         = Vertex Class "Mapping" (P3 1 3 1) [setter]
      }

data Diagram a = Diagram
  { iso             :: a
  , lens            :: a
  , getter          :: a
  , prism           :: a
  , review          :: a
  , optional        :: a
  , affineFold      :: a
  , traversal       :: a
  , fold            :: a
  , setter          :: a
  , profunctor      :: a
  , strong          :: a
  , cochoice        :: a
  , bicontravariant :: a
  , choice          :: a
  , costrong        :: a
  , bifunctor       :: a
  , closed          :: a
  , traversing      :: a
  , mapping         :: a
  }
  deriving (Foldable, Functor, Traversable)
