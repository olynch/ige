module IGE.Layout
  ( layoutGr )
  where

import Protolude hiding (force)
import IGE.Types
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Map.Strict as Map
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Data.STRef
import Lens.Micro.Platform
import System.Random.MWC

data PEdge = Spring Double Double | Spacer Double Double

force :: PEdge -> ℂ -> ℂ -> ℂ
force (Spring k l) p1 p2 = ((k * (magnitude z - l)) :+ 0) * signum z
  where z = p2 - p1
force (Spacer k l) p1 p2 = ((max 0 (k * (magnitude z - l))) :+ 0) * signum z
  where z = p2 - p1

type Edges = Array (Int, Int) PEdge
type STNodes s = STArray s Int ℂ
type Nodes = Array Int ℂ

fdStep :: Double -> Edges -> STNodes s -> ST s Double
fdStep stepSize edges nodes = do
  let stepSizeC = stepSize :+ 0
  (0, n) <- getBounds nodes
  maxForceRef <- newSTRef 0
  forM_ [0..n] $ \i -> do
    fRef <- newSTRef $ 0 :+ 0
    p1 <- readArray nodes i
    forM_ [0..n] $ \j -> do
      p2 <- readArray nodes j
      let edge = edges ! (i, j)
      f <- readSTRef fRef
      writeSTRef fRef $ f + (force edge p1 p2)
    f <- readSTRef fRef
    let dp = stepSizeC * f
    writeArray nodes i $ p1 + dp
    maxF <- readSTRef maxForceRef
    if magnitude f > maxF
      then writeSTRef maxForceRef $ magnitude f
      else return ()
  maxF <- readSTRef maxForceRef
  return maxF

runFd :: Double -> Double -> Edges -> STNodes s -> ST s ()
runFd tolerance stepSize edges nodes = perturb >> loop
  where
    perturb = do
      gen <- create
      (0, n) <- getBounds nodes
      forM_ [0..n] $ \i -> do
        p <- readArray nodes i
        cθ <- uniformR (0, 2 * pi) gen
        cr <- uniformR (0, 0.2) gen
        writeArray nodes i (p + mkPolar cr cθ)
    loop = do
      f <- fdStep stepSize edges nodes
      if f > tolerance
        then loop
        else return ()

defaultSpring :: PEdge
defaultSpring = Spring 0.1 0.8
defaultSpacer :: PEdge
defaultSpacer = Spacer 0 0.5
defaultStepSize :: Double
defaultStepSize = 0.01
defaultTolerance :: Double
defaultTolerance = 0.001

initNodes :: Gr a b -> (Map.Map Node Int, Nodes)
initNodes g = ( Map.fromList $ zip (nodes g) [0..]
              , listArray (0, n - 1) $ (\k -> fromIntegral (k `rem` d) :+ fromIntegral (k `quot` d)) <$> [0..n-1])
  where
    n = length $ nodes g
    d = ceiling $ sqrt $ fromIntegral $ order g

initEdges :: Gr a b -> Map.Map Node Int -> Edges
initEdges g m = runSTArray $ do
    edgeMat <- newArray ((0, 0), (n - 1, n - 1)) defaultSpacer
    forM_ (over both (m Map.!) <$> edges g) $ \(n1, n2) -> do
      writeArray edgeMat (n1, n2) defaultSpring
      writeArray edgeMat (n2, n1) defaultSpring
    return edgeMat
  where
    n = length $ nodes g

layoutGr :: Gr a b -> Map.Map Node ℂ
layoutGr g = Map.map (nodeVec !) nodeMap
  where
    (nodeMap, frozenNodes) = initNodes g
    nodeVec = runSTArray $ do
      let edgeMat = initEdges g nodeMap
      nodeVec' <- thaw frozenNodes
      runFd defaultTolerance defaultStepSize edgeMat nodeVec'
      return nodeVec'
