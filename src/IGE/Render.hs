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

-- Refactoring ideas:
-- - Use diagrams

instance Renderable Text where
  render s (x :+ y) = do
    extents <- textExtents s
    rectangle
      (x - textMargin)
      (y - (textExtentsHeight extents + textMargin))
      (textExtentsWidth extents + (2 * textMargin))
      (textExtentsHeight extents + (2 * textMargin))
    setSourceRGB 0.1 0.1 0.1
    fill
    setSourceRGB 0.9 0.9 0.9
    moveTo x y
    showText s

instance Renderable () where
  render () (x :+ y) = do
    arc x y nodeSize 0 (2 * pi)
    fill

instance RenderNode Text where

instance RenderEdge Text where

instance RenderNode () where

instance RenderEdge () where
  renderEdge _ _ = return ()

nodeSize :: Double
nodeSize = 4

renderBackground :: Render ()
renderBackground = do
  setSourceRGB 0.1 0.1 0.1
  paint
  setSourceRGB 0.9 0.9 0.9
  selectFontFace ("monospace" :: Text) FontSlantNormal FontWeightNormal
  setFontSize 15

renderNodes :: (RenderNode a) => [(ℂ, a)] -> Render ()
renderNodes nodes = mapM_ (uncurry $ flip renderNode) nodes

renderEdges :: (RenderEdge a) => [(ℂ, ℂ, a)] -> Render ()
renderEdges edges =
  forM_ edges $ \(p1@(x0 :+ y0), p2@(x1 :+ y1), label) -> do
    moveTo x0 y0
    lineTo x1 y1
    stroke
    renderEdge label $ (p1 + p2) / (2 :+ 0)

renderCommand :: (Int, Int) -> [Char] -> Render ()
renderCommand (w, h) s = do
  extents <- textExtents s
  moveTo 0 ((fromIntegral h) - (textExtentsHeight extents))
  showText s

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

renderEditorState :: (RenderNode n, RenderEdge e) => EditorState n e -> (Int, Int) -> Render ()
renderEditorState es dims = do
  let rm = es^._rm
  let graph = es^._graph
  let labels = es^._labels
  let nodeMap = (rm ^*) <$> es^._nodeMap
  renderBackground
  renderEdges $ ((_1 %~ (nodeMap Map.!)) . (_2 %~ (nodeMap Map.!))) <$> labEdges graph
  renderNodes $ (_1 %~ (nodeMap Map.!)) <$> labNodes graph
  renderCommand dims (es^._prompt ++ reverse es^._cmd)
  renderLabels $ over _2 (nodeMap Map.!) <$> labels
