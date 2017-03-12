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

data Connection = Connection { guiexec :: FilePath, commandFifo :: FilePath, eventFifo :: FilePath }

argumentParser :: Parser Connection
argumentParser = Connection
  <$> argument str
      ( metavar "GUIEXEC"
     <> help "name of executable to run")
  <*> strOption
      ( long "commandfifo"
     <> short 'c'
     <> metavar "COMMANDFIFO"
     <> value "commands"
     <> showDefault
     <> help "name of fifo to be created or used to input commands")
  <*> strOption
      ( long "eventfifo"
     <> short 'e'
     <> metavar "EVENTFIFO"
     <> value "events"
     <> showDefault
     <> help "name of fifo to be created or used to read events")

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> argumentParser) fullDesc
  commandSink <- openFile (commandFifo opts) AppendMode
  eventSource <- openFile (eventFifo opts) ReadMode
  eventStr <- B.hGetLine eventSource
  let cmd = (decode $ BL.fromStrict eventStr :: Maybe Event)
  putStrLn $ "deserialized = " ++ (show cmd)
  BL.hPutStrLn commandSink $ encode cmd
  hClose commandSink
  hClose eventSource
