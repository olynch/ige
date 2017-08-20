module Main where

import Protolude
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import IGE.Types
import IGE.UI
import IGE.Control

sampleGr :: Gr () ()
sampleGr = mkGraph [(0, ()), (1, ()), (2, ())] [(0, 1, ()), (1, 2, ()), (0, 2, ())]

initRM :: RM
initRM = RM (100 :+ 0) (100 :+ 100)

main :: IO ()
main = runMainWindow sampleGr initRM basicKeyBinding
