module IGE.Layout
  ( renderGr )
  where

import Protolude
import IGE.Types
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

renderGr :: Gr a b -> Gr ℂ () 
renderGr g = mkGraph newNodes newEdges
  where
    newNodes = map (\n -> (n, fromIntegral (n `rem` d) :+ fromIntegral (n `quot` d))) $ nodes g
    newEdges = map (\(n0, n1) -> (n0, n1, ())) $ edges g
    d = ceiling $ sqrt $ fromIntegral $ order g
