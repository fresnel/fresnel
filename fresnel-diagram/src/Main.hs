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
, P4(..)
) where

import           Control.Applicative (liftA2)
import           Control.Monad (join, unless)
import           Data.Foldable as Foldable (fold, for_)
import           Data.Function (fix)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           Text.Blaze.Svg.Renderer.Pretty
import           Text.Blaze.Svg11 as S hiding (z)
import qualified Text.Blaze.Svg11.Attributes as A

main :: IO ()
main = do
  let opts =
        [ Option ['c'] ["css"] (ReqArg CSS "FILE") "path to CSS source to embed"
        , Option ['j'] ["js"] (ReqArg JS "FILE") "path to JS source to embed"
        ]
  (vals, out, errs) <- getOpt RequireOrder opts <$> getArgs
  case errs of
    [] -> pure ()
    _  -> ioError (userError (concat errs ++ usageInfo header opts))
  rendered <- renderSvg <$> renderDiagram graph vals
  case out of
    []     -> putStrLn rendered
    path:_ -> writeFile path rendered
  where
  header = "Usage: fresnel-diagram [-c FILE|--css FILE] [FILE]"

data Opt a = CSS a | JS a
  deriving (Foldable, Functor, Traversable)

renderDiagram :: Diagram Vertex -> [Opt FilePath] -> IO Svg
renderDiagram diagram opts = do
  opts' <- traverse (traverse readFile) opts
  pure $ svg ! A.version "1.1" ! xmlns "http://www.w3.org/2000/svg" ! A.viewbox "-575 -150 1300 650" $ do
    foldMap (\case
      CSS s -> S.style (toMarkup s)
      _     -> mempty) opts'
    let (vertices, gradients) = traverse renderVertex diagram
    defs (Foldable.fold gradients)
    vertices
    foldMap (\case
      JS s -> S.script (toMarkup s)
      _    -> mempty) opts'

renderVertex :: Vertex -> (Svg, Svg)
renderVertex Vertex{ kind, name, coords = coords@P3{ x }, labelPos = P2 ex ey, outEdges } = (do
  let p = scale (project coords)
  g ! A.id_ (stringValue name) ! A.class_ (stringValue ("vertex " <> show kind)) ! A.transform (uncurryP2 translate p) $ do
    for_ outEdges $ \ (Dest offset Vertex{ name = dname, coords = dcoords }) ->
      S.path ! A.id_ (stringValue (name <> "-" <> dname)) ! A.class_ (stringValue (unwords ["edge", show kind, name, dname])) ! A.d (mkPath (edge coords dcoords)) !? maybe (False, mempty) ((,) True . A.transform . uncurryP2 translate) offset
    circle ! A.r "2.5"
    path ! A.class_ "label" ! A.d (mkPath labelEdge)
    text_ ! A.transform (uncurryP2 translate labelOffset) $ toMarkup name, defs)
  where
  project (P3 x y z) = P2 (negate x + y) (x + y - z)
  scale (P2 x y) = P2 (x * 200) (y * 100)
  labelEdge = do
    mr 0 (0 :: Int)
    lr (sige ex * 30) (sige ey * 15)
    hr (sige ex * 50)
  labelOffset = P2 (sige ex * 80) (sige ey * 30 :: Int)
  sige = \case
    Just Min -> -1 :: Int
    Nothing  -> 0
    Just Max -> 1
  defs = case kind of
    Optic -> foldMap edge outEdges where
      edge (Dest _ Vertex{ name = dname, coords = P3{ x = dx } }) =
        let x1 = if dx > x then "100%" else "0%"
            x2 = if dx > x then "0%" else "100%"
            y1 = "0%"
            y2 = "100%"
        in lineargradient ! A.id_ (stringValue ("gradient-" <> name <> "-" <> dname)) ! A.x1 x1 ! A.y1 y1 ! A.x2 x2 ! A.y2 y2 $ do
          stop ! A.class_ (stringValue name) ! A.offset "0%"
          stop ! A.class_ (stringValue dname) ! A.offset "100%"
    Class -> mempty

xmlns = customAttribute "xmlns"

edge :: P3 Int -> P3 Int -> Path
edge s e = edgeX
  where
  P3 dx dy dz = e - s
  voffset = P2 0 10
  project (P3 x y z) = P2 (negate x + y) (x + y - z)
  scale (P2 x y) = P2 (x * 200) (y * 100)
  edgeX | dx == 0   = edgeY False
        | otherwise = do
    let δ = scale (project (P3 dx 0 0))
        sδ = signum δ
        hoffset = P2 (-10) 5
        isLast = dy == 0 && dz == 0
    uncurryP2 mr hoffset
    uncurryP2 lr (δ - hoffset * if isLast then 2 else 1)
    if isLast then do
      uncurryP2 mr (sδ * P2 (-6) (-8))
      uncurryP2 lr (sδ * P2 6 8)
      uncurryP2 lr (sδ * P2 (-10) 0)
    else
      edgeY True
  edgeY c | dy == 0   = edgeZ c
          | otherwise = do
    let δ = scale (project (P3 0 dy 0))
        sδ = signum δ
        hoffset = P2 10 5
    unless c $ uncurryP2 mr (sδ * hoffset)
    uncurryP2 lr (δ - sδ * hoffset * if c then 1 else 2)
    if dz /= 0 then
      edgeZ True
    else do
      uncurryP2 mr (sδ * P2 (-6) (-8))
      uncurryP2 lr (sδ * P2 6 8)
      uncurryP2 lr (sδ * P2 (-10) 0)
  edgeZ c = do
    unless c $ uncurryP2 mr voffset
    let δ = scale (project (P3 0 0 dz))
    uncurryP2 lr (δ - voffset * if dx /= 0 || dy /= 0 then 1 else 2)
    -- FIXME: invert when the edge goes up
    uncurryP2 mr (P2 (-4.47213595499958) (-8.94427190999916) :: P2 Float)
    uncurryP2 lr (P2 4.47213595499958 8.94427190999916 :: P2 Float)
    uncurryP2 lr (P2 4.47213595499958 (-8.94427190999916) :: P2 Float)

data Vertex = Vertex
  { kind     :: VertexKind
  , name     :: String
  , coords   :: P3 Int
  , labelPos :: P2 (Maybe Extent)
  , outEdges :: [Dest]
  }

data VertexKind
  = Optic
  | Class
  deriving (Eq, Ord, Show)

data Extent = Min | Max

data P4 a = P4 { x :: a, y :: a, z :: a, w :: a }
  deriving (Functor)

instance Applicative P4 where
  pure = join (join (join P4))
  P4 f1 f2 f3 f4 <*> P4 a1 a2 a3 a4 = P4 (f1 a1) (f2 a2) (f3 a3) (f4 a4)

instance Num a => Num (P4 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

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
  deriving (Functor, Show)

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

data Dest = Dest (Maybe (P2 Float)) Vertex

dest :: Vertex -> Dest
dest = Dest Nothing

offset :: P2 Float -> Vertex -> Dest
offset = Dest . Just

graph :: Diagram Vertex
graph = fix $ \ ~Diagram{ iso, lens, getter, prism, review, optional, affineFold, traversal, fold, setter, strong, cochoice, choice, costrong, closed, traversing, mapping } -> Diagram
  { iso             = Vertex Optic "Iso"             (P3 0 0 0) (P2 (Just Max) (Just Min)) [dest lens, dest prism]
  , lens            = Vertex Optic "Lens"            (P3 1 0 0) (P2 (Just Min) (Just Min)) [dest optional, dest getter]
  , getter          = Vertex Optic "Getter"          (P3 2 0 0) (P2 (Just Min) (Just Max)) [dest affineFold]
  , prism           = Vertex Optic "Prism"           (P3 0 1 0) (P2 (Just Max) (Just Min)) [dest optional, dest review]
  , review          = Vertex Optic "Review"          (P3 0 2 0) (P2 (Just Max) (Just Min)) []
  , optional        = Vertex Optic "Optional"        (P3 1 1 0) (P2 (Just Min) Nothing)    [dest affineFold, dest traversal]
  , affineFold      = Vertex Optic "AffineFold"      (P3 2 1 0) (P2 (Just Min) (Just Max)) [dest fold]
  , traversal       = Vertex Optic "Traversal"       (P3 1 2 0) (P2 (Just Max) (Just Min)) [dest fold, dest setter]
  , fold            = Vertex Optic "Fold"            (P3 2 2 0) (P2 (Just Min) (Just Max)) []
  , setter          = Vertex Optic "Setter"          (P3 1 3 0) (P2 (Just Max) (Just Max)) []
  , profunctor      = Vertex Class "Profunctor"      (P3 0 0 1) (P2 (Just Max) (Just Min)) [dest iso, offset ny strong, offset (px * 2) choice, offset py cochoice, dest costrong, offset (nx * 2) closed]
  , strong          = Vertex Class "Strong"          (P3 1 0 1) (P2 (Just Min) (Just Min)) [dest lens, offset px traversing]
  , cochoice        = Vertex Class "Cochoice"        (P3 2 0 1) (P2 (Just Min) (Just Min)) [offset px getter]
  , bicontravariant = Vertex Class "Bicontravariant" (P3 2 0 2) (P2 (Just Min) (Just Min)) [offset nx getter]
  , choice          = Vertex Class "Choice"          (P3 0 1 1) (P2 (Just Max) (Just Min)) [dest prism, offset nx traversing]
  , costrong        = Vertex Class "Costrong"        (P3 0 2 1) (P2 (Just Max) (Just Min)) [offset py review]
  , bifunctor       = Vertex Class "Bifunctor"       (P3 0 2 2) (P2 (Just Max) (Just Min)) [offset ny review]
  , closed          = Vertex Class "Closed"          (P3 0 3 1) (P2 (Just Max) (Just Min)) [dest mapping]
  , traversing      = Vertex Class "Traversing"      (P3 1 2 1) (P2 (Just Min) (Just Max)) [dest traversal, dest mapping]
  , mapping         = Vertex Class "Mapping"         (P3 1 3 1) (P2 (Just Max) (Just Max)) [dest setter]
  }
  where
  P4 ny px nx py = (P2 5 2.5 *) <$> (P2 <$> P4 (-1) (-1) 1 1 <*> P4 (-1) 1 (-1) 1)

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
