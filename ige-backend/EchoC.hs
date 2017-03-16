{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import Data.Text
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Options.Applicative
import System.IO
import System.Posix.IO (handleToFd)
import Control.Concurrent (threadWaitRead)
import Data.Semigroup ((<>))
import Network.Socket hiding (send, recv, sendTo, recvFrom)
import Network.Socket.ByteString

data Command =
  AddNode
  | AddEdge { nodeId1 :: Int, nodeId2 :: Int }
  | DelNode { cmdNodeId :: Int }
  | BeginSelection
  deriving (Generic, Show)

instance ToJSON Command
instance FromJSON Command

data Event =
  KeyPress { key :: Int, modifier :: Int }
  | Command { cmd :: Text }
  | Selection { selNodeId :: Int }
  deriving (Generic, Show)

instance FromJSON Event
instance ToJSON Event

data Connection = Connection { socketPath :: FilePath }

argumentParser :: Parser Connection
argumentParser = Connection
  <$> argument str
      ( metavar "SOCKETPATH"
     <> help "path to socket")

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> argumentParser) fullDesc
  let addr = SockAddrUnix $ socketPath opts
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock addr
  sendAll sock (BL.toStrict $ encode $ AddEdge 1 2)
  response <- recv sock 100
  print response
