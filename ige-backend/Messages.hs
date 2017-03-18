{-# LANGUAGE DeriveGeneric #-}

module Messages
(
  ControlMsg(..),
  Event(..)
)
where

import Data.MessagePack
import GHC.Generics
import Data.Binary


data ControlMsg =
  AddNode
  | DelNode { nodeId :: Int }
  | AddEdge { nodeId1 :: Int, nodeId2 :: Int }
  | DelEdge { edgeId :: Int }
  | GetSelection
  | Zoom { percent :: Double }
  | Rotate { radians :: Double }
  | Translate { x :: Double, y :: Double }
  deriving (Show, Eq, Generic)

instance MessagePack ControlMsg where

instance Binary ControlMsg where
  get = get >>= fromObject
  put = put . toObject

data Event =
  KeyPress { key :: Int, modifier :: Int }
  | Selection { selNodeId :: Int }
  deriving (Show, Eq, Generic)

instance MessagePack Event where

instance Binary Event where
  get = get >>= fromObject
  put = put . toObject
