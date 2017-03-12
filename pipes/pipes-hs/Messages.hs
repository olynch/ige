module Messages
(
  Command(..),
  Event(..)
)
where

import Data.MessagePack
import Data.Serialize
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T

data Command =
  AddNode
  | AddEdge { nodeId1 :: Int, nodeId2 :: Int }
  | DelNode { cmdNodeId :: Int }
  | BeginSelection
  deriving (Show, Eq)

instance Serialize Command where
  put (AddNode) = put $ ObjectArray
    [ObjectInt 0, ObjectArray []]
  put (AddEdge n1 n2) = put $ ObjectArray
    [ObjectInt 1, ObjectArray [ObjectInt $ fromIntegral n1, ObjectInt $ fromIntegral n2]]
  put (DelNode n1) = put $ ObjectArray
    [ObjectInt 2, ObjectArray [ObjectInt $ fromIntegral n1]]
  put (BeginSelection) = put $ ObjectArray
    [ObjectInt 3, ObjectArray []]
  get = get >>= getCommand
    where getCommand (ObjectArray [ObjectInt 0, ObjectArray []]) = return AddNode
          getCommand (ObjectArray [ObjectInt 1, ObjectArray [ObjectInt n1, ObjectInt n2]]) = return $ AddEdge (fromIntegral n1) (fromIntegral n2)
          getCommand (ObjectArray [ObjectInt 2, ObjectArray [ObjectInt n1]]) = return $ DelNode (fromIntegral n1)
          getCommand (ObjectArray [ObjectInt 3, ObjectArray []]) = return BeginSelection
          getCommand o = fail $ "unsupported command: " ++ show o

data Event =
  KeyPress { key :: Int, modifier :: Int }
  | Command { cmd :: T.Text }
  | Selection { selNodeId :: Int }
  deriving (Show, Eq)

instance Serialize Event where
  put (KeyPress key modifier) = put $ ObjectArray
    [ObjectInt 0, ObjectArray [ObjectInt $ fromIntegral key, ObjectInt $ fromIntegral modifier]]
  put (Command cmdStr) = put $ ObjectArray
    [ObjectInt 1, ObjectArray [ObjectString $ encodeUtf8 cmdStr]]
  put (Selection nid) = put $ ObjectArray
    [ObjectInt 2, ObjectArray [ObjectInt $ fromIntegral nid]]
  get = get >>= getEvent
    where getEvent (ObjectArray [ObjectInt 0, ObjectArray [ObjectInt key, ObjectInt modifier]]) =
            return $ KeyPress (fromIntegral key) (fromIntegral modifier)
          getEvent (ObjectArray [ObjectInt 1, ObjectArray [ObjectString cmdStr]]) =
            return $ Command $ decodeUtf8 cmdStr
          getEvent (ObjectArray [ObjectInt 2, ObjectArray [ObjectInt nid]]) =
            return $ Selection (fromIntegral nid)
          getEvent o = fail $ "unsupported command: " ++ show o
