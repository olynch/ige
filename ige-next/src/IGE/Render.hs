module IGE.Render
  ( renderEditorState )
  where

import Protolude
import IGE.Types
import IGE.Layout
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Graphics.Rendering.Cairo
import Lens.Micro.Platform
import qualified Data.Map.Strict as Map

nodeSize :: Double
nodeSize = 4

renderBackground :: Render ()
renderBackground = do
  setSourceRGB 0.1 0.1 0.1
  paint
  setSourceRGB 0.9 0.9 0.9
  selectFontFace "monospace" FontSlantNormal FontWeightNormal
  setFontSize 15

renderNodes :: [ℂ] -> Render ()
renderNodes nodes = do
  forM_ nodes $ \(x :+ y) -> do
    arc x y nodeSize 0 (2 * pi)
    fill

renderEdges :: [(ℂ, ℂ)] -> Render ()
renderEdges edges =
  forM_ edges $ \((x0 :+ y0), (x1 :+ y1)) -> do
    moveTo x0 y0
    lineTo x1 y1
    stroke

renderCommand :: (Int, Int) -> [Char] -> Render ()
renderCommand (w, h) s = do
  let s' = reverse s
  extents <- textExtents s'
  moveTo 0 ((fromIntegral h) - (textExtentsHeight extents))
  showText s'

textMargin = 3

renderLabels :: [([Char], ℂ)] -> Render ()
renderLabels labels =
  forM_ labels $ \(s, x :+ y) -> do
    extents <- textExtents s
    rectangle
      (x - textMargin)
      (y - (textExtentsHeight extents + textMargin))
      (textExtentsWidth extents + (2 * textMargin))
      (textExtentsHeight extents + (2 * textMargin))
    setSourceRGB 0.7 0.7 0.2
    fill
    setSourceRGB 0.1 0.1 0.1
    moveTo x y
    showText s

renderEditorState :: EditorState -> (Int, Int) -> Render ()
renderEditorState es dims = do
  let rm = es^._rm
  let graph = es^._graph
  let labels = es^._labels
  let nodeMap = (rm ^*) <$> es^._nodeMap
  renderBackground
  renderNodes $ (nodeMap Map.!) <$> nodes graph
  renderEdges $ over both (nodeMap Map.!) <$> edges graph
  renderCommand dims (es^._cmd)
  renderLabels $ over _2 (nodeMap Map.!) <$> labels
