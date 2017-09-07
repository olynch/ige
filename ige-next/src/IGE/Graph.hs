module Graph where

import Protolude
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Lens.Micro
import qualified Data.Map as Map

-- Embedded Graph
type EGr n e = Gr (ℂ, n) e
