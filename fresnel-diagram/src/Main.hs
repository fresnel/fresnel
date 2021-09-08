{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main
( main
, Vertex(..)
) where

import           Control.Monad (guard, unless)
import           Data.Foldable as Foldable (fold, for_)
import qualified Data.Map as Map
import           Linear.V2
import           Linear.V3
import           Linear.V4
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
renderVertex v@Vertex{ kind, name, coords = coords@(V3 x _ _), labelPos = V2 ex ey, outEdges } = (do
  let p = scale (project coords)
  g ! A.id_ (stringValue name) ! A.class_ (stringValue ("vertex " <> show kind)) ! A.transform (uncurryV2 translate p) $ do
    for_ outEdges (edgeElement v)
    circle ! A.r "2.5"
    path ! A.class_ "label" ! A.d (mkPath labelEdge)
    text_ ! A.transform (uncurryV2 translate labelOffset) $ toMarkup name
    g ! A.class_ "ancestors" $ do
      Foldable.fold (ancestors v)
  , defs)
  where
  project (V3 x y z) = V2 (negate x + y) (x + y - z)
  scale (V2 x y) = V2 (x * 200) (y * 100)
  labelEdge = do
    mr 0 (0 :: Int)
    lr (sige ex * 30) (sige ey * 15)
    hr (sige ex * 50)
  labelOffset = V2 (sige ex * 80) (sige ey * 30 :: Int)
  sige = \case
    Just Min -> -1 :: Int
    Nothing  -> 0
    Just Max -> 1
  defs = case kind of
    Optic -> foldMap edge outEdges where
      edge (_, Vertex{ name = dname, coords = V3 dx _ _ }) =
        let x1 = if dx > x then "100%" else "0%"
            x2 = if dx > x then "0%" else "100%"
            y1 = "0%"
            y2 = "100%"
        in lineargradient ! A.id_ (stringValue ("gradient-" <> name <> "-" <> dname)) ! A.x1 x1 ! A.y1 y1 ! A.x2 x2 ! A.y2 y2 $ do
          stop ! A.class_ (stringValue name) ! A.offset "0%"
          stop ! A.class_ (stringValue dname) ! A.offset "100%"
    Class -> mempty

ancestors :: Vertex -> Map.Map String Svg
ancestors u = go 0 Map.empty u where
  go :: V2 Float -> Map.Map String Svg -> Vertex -> Map.Map String Svg
  go offset accum u = case Map.lookup (Main.name u) accum of
    Just _  -> mempty
    Nothing -> foldMap inEdge (inEdges u) where
      inEdge (v, _) = Map.insert id' use' (go offset accum' v) where
        id' = edgeId v u
        use' = use
          ! href (stringValue ('#':id'))
          ! A.class_ (edgeClass v u)
          ! A.transform (uncurryV2 translate offset')
        offset' = negate (edgeOffset (coords v) (coords u))
      accum' = Map.insert (Main.name u) mempty accum

edgeElement :: Vertex -> (Maybe (V2 Float), Vertex) -> Svg
edgeElement u (offset, v) = S.path
  ! A.id_ (stringValue (edgeId u v))
  ! A.class_ (edgeClass u v)
  ! A.d (mkPath (edgePath (coords u) (coords v)))
  !?? (A.transform . uncurryV2 translate <$> offset)

edgeId :: Vertex -> Vertex -> String
edgeId Vertex{ name = a } Vertex{ name = b } = a <> "-" <> b

edgeClass :: Vertex -> Vertex -> AttributeValue
edgeClass u v = stringValue (unwords ["edge", show (kind u), name u, name v])

xmlns = customAttribute "xmlns"
href = customAttribute "href"

(!??) :: Svg -> Maybe Attribute -> Svg
s !?? a = s !? maybe (False, mempty) (True,) a

edgePath :: V3 Int -> V3 Int -> Path
edgePath s e = edgeX
  where
  V3 dx dy dz = e - s
  voffset = V2 0 10
  project (V3 x y z) = V2 (negate x + y) (x + y - z)
  scale (V2 x y) = V2 (x * 200) (y * 100)
  edgeX | dx == 0   = edgeY False
        | otherwise = do
    let δ = scale (project (V3 dx 0 0))
        sδ = signum δ
        hoffset = V2 (-10) 5
        isLast = dy == 0 && dz == 0
    uncurryV2 mr hoffset
    uncurryV2 lr (δ - hoffset * if isLast then 2 else 1)
    if isLast then do
      uncurryV2 mr (sδ * V2 (-6) (-8))
      uncurryV2 lr (sδ * V2 6 8)
      uncurryV2 lr (sδ * V2 (-10) 0)
    else
      edgeY True
  edgeY c | dy == 0   = edgeZ c
          | otherwise = do
    let δ = scale (project (V3 0 dy 0))
        sδ = signum δ
        hoffset = V2 10 5
    unless c $ uncurryV2 mr (sδ * hoffset)
    uncurryV2 lr (δ - sδ * hoffset * if c then 1 else 2)
    if dz /= 0 then
      edgeZ True
    else do
      uncurryV2 mr (sδ * V2 (-6) (-8))
      uncurryV2 lr (sδ * V2 6 8)
      uncurryV2 lr (sδ * V2 (-10) 0)
  edgeZ c = do
    unless c $ uncurryV2 mr voffset
    let δ = scale (project (V3 0 0 dz))
    uncurryV2 lr (δ - voffset * if dx /= 0 || dy /= 0 then 1 else 2)
    -- FIXME: invert when the edge goes up
    -- FIXME: calculate these from the arrowhead geometry
    uncurryV2 mr (V2 (-4.47213595499958) (-8.94427190999916) :: V2 Float)
    uncurryV2 lr (V2 4.47213595499958 8.94427190999916 :: V2 Float)
    uncurryV2 lr (V2 4.47213595499958 (-8.94427190999916) :: V2 Float)

edgeOffset :: V3 Int -> V3 Int -> V2 Float
edgeOffset s e = realToFrac <$> scale (project (e - s))
  where
  project (V3 x y z) = V2 (negate x + y) (x + y - z)
  scale (V2 x y) = V2 (x * 200) (y * 100)


data Vertex = Vertex
  { kind     :: VertexKind
  , name     :: String
  , coords   :: V3 Int
  , labelPos :: V2 (Maybe Extent)
  , inEdges  :: [(Vertex, Maybe (V2 Float))]
  , outEdges :: [(Maybe (V2 Float), Vertex)]
  }

data VertexKind
  = Optic
  | Class
  deriving (Eq, Ord, Show)

data Extent = Min | Max

uncurryV2 :: (a -> a -> b) -> (V2 a -> b)
uncurryV2 f (V2 x y) = f x y

dest :: Vertex -> (Maybe (V2 Float), Vertex)
dest = (Nothing,)

offset :: V2 Float -> Vertex -> (Maybe (V2 Float), Vertex)
offset = (,) . Just

graph :: Diagram Vertex
graph = diagram
  where
  diagram = Diagram{ iso, lens, getter, prism, review, optional, affineFold, traversal, fold, setter, profunctor, strong, cochoice, bicontravariant, choice, costrong, bifunctor, closed, traversing, mapping }
  iso             = optic "Iso"             (V3 0 0 0) (V2 mx mn) [dest lens, dest prism]
  lens            = optic "Lens"            (V3 1 0 0) (V2 mn mn) [dest optional, dest getter]
  getter          = optic "Getter"          (V3 2 0 0) (V2 mn mx) [dest affineFold]
  prism           = optic "Prism"           (V3 0 1 0) (V2 mx mn) [dest optional, dest review]
  review          = optic "Review"          (V3 0 2 0) (V2 mx mn) []
  optional        = optic "Optional"        (V3 1 1 0) (V2 mn no) [dest affineFold, dest traversal]
  affineFold      = optic "AffineFold"      (V3 2 1 0) (V2 mn mx) [dest fold]
  traversal       = optic "Traversal"       (V3 1 2 0) (V2 mx mn) [dest fold, dest setter]
  fold            = optic "Fold"            (V3 2 2 0) (V2 mn mx) []
  setter          = optic "Setter"          (V3 1 3 0) (V2 mx mx) []
  profunctor      = klass "Profunctor"      (V3 0 0 1) (V2 mx mn) [dest iso, offset ny strong, offset (px * 2) choice, offset py cochoice, dest costrong, offset (nx * 2) closed]
  strong          = klass "Strong"          (V3 1 0 1) (V2 mn mn) [dest lens, offset px traversing]
  cochoice        = klass "Cochoice"        (V3 2 0 1) (V2 mn mn) [offset px getter]
  bicontravariant = klass "Bicontravariant" (V3 2 0 2) (V2 mn mn) [offset nx getter]
  choice          = klass "Choice"          (V3 0 1 1) (V2 mx mn) [dest prism, offset nx traversing]
  costrong        = klass "Costrong"        (V3 0 2 1) (V2 mx mn) [offset py review]
  bifunctor       = klass "Bifunctor"       (V3 0 2 2) (V2 mx mn) [offset ny review]
  closed          = klass "Closed"          (V3 0 3 1) (V2 mx mn) [dest mapping]
  traversing      = klass "Traversing"      (V3 1 2 1) (V2 mn mx) [dest traversal, dest mapping]
  mapping         = klass "Mapping"         (V3 1 3 1) (V2 mx mx) [dest setter]
  optic name p l = Vertex Optic name p l (parents name)
  klass name p l = Vertex Class name p l (parents name)
  parents n = foldMap (\ v@Vertex{ outEdges } -> foldMap (\ (o, Vertex{ name }) -> (v, o) <$ guard (name == n)) outEdges) diagram
  mn = Just Min
  mx = Just Max
  no = Nothing
  V4 ny px nx py = (V2 5 2.5 *) <$> (V2 <$> V4 (-1) (-1) 1 1 <*> V4 (-1) 1 (-1) 1)

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
