{-# LANGUAGE DeriveGeneric #-}

module Messages
(
  ControlMsg(..),
  DisplayCommand(..),
  GraphCommand(..),
  Event(..)
)
where

import Data.MessagePack
import GHC.Generics
import Data.Binary


data ControlMsg =
  DispMsg DisplayCommand
  | GraphMsg GraphCommand
  deriving (Show, Eq, Generic)

instance MessagePack ControlMsg where

instance Binary ControlMsg where
  get = get >>= fromObject
  put = put . toObject

data GraphCommand =
  AddNode
  | DelNode { nodeId :: Int }
  | AddEdge { nodeId1 :: Int, nodeId2 :: Int }
  | DelEdge { edgeId :: Int }
  deriving (Show, Eq, Generic)

instance MessagePack GraphCommand where

instance Binary GraphCommand where
  get = get >>= fromObject
  put = put . toObject

data DisplayCommand =
  Refresh
  | Zoom { percent :: Double }
  | Rotate { radians :: Double }
  | Translate { x :: Double, y :: Double }
  | GetSelection
  deriving (Show, Eq, Generic)

instance MessagePack DisplayCommand where

instance Binary DisplayCommand where
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
