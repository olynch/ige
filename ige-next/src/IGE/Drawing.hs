module IGE.Drawing 
  ( renderEditorState )
  where

import Protolude
import IGE.Types
import IGE.Layout
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Graphics.Rendering.Cairo
import Lens.Micro.Platform

nodeSize :: Double
nodeSize = 4

drawBackground :: Render ()
drawBackground = do
  setSourceRGB 0.1 0.1 0.1
  paint

edgeLabels :: Gr a b -> [(a, a)]
edgeLabels graph = catMaybes $ uncurry (liftA2 (,)) . over both (lab graph) <$> edges graph

drawGr :: Gr ℂ () -> Render ()
drawGr g = do
  setSourceRGB 0.9 0.9 0.9
  forM_ (labNodes g) $ \(_, (x :+ y)) -> do
    arc x y nodeSize 0 (2 * pi)
    fill
  forM_ (edgeLabels g) $ \((x0 :+ y0), (x1 :+ y1)) -> do
    moveTo x0 y0
    lineTo x1 y1
    stroke

-- drawCommand :: String -> Render ()
-- drawCommand s = do
--   setSourceRGB 0.9 0.9 0.9

-- drawLabels :: Gr ℂ () -> [(String, Node)] -> Render ()
-- drawLabels gr 

renderEditorState :: EditorState -> Render ()
renderEditorState es = do
  drawBackground
  drawGr $ nmap ((es^._rm) ^*) $ renderGr (es^._graph)
  -- drawCommand (es^._cmd)
  -- drawLabels (es^._graph) (es^._labels)
