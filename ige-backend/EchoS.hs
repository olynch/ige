{-# LANGUAGE DeriveGeneric #-}

import Messages
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Control.Monad (unless)
import Data.Semigroup ((<>))
import Network.Socket hiding (send, recv, sendTo, recvFrom)
import Network.Socket.ByteString
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize
import Data.Conduit.Cereal
import Data.Conduit.Network.Unix
import Conduit

data Connection = Connection { socketPath :: FilePath }

argumentParser :: Parser Connection
argumentParser = Connection
  <$> argument str
      ( metavar "SOCKETPATH"
     <> help "path to socket")

echo :: Socket -> IO ()
echo s = do
  request <- recv s 100
  unless (B.null request) $ sendAll s request >> echo s

inspect :: (Show a) => a -> IO a
inspect a = do
  print a
  return a

{-processEvents :: ConduitM Event Command IO ()-}
{-processEvents = do-}
{-  ev <- takeC 1-}
{-  case ev of-}
{-    (KeyPress key modifier) -> -}

echoC :: AppDataUnix -> IO ()
echoC ad = do
  runConduit $
    appSource ad
    .| mapMC inspect
    .| conduitGet2 (get :: Get Command)
    {-.| mapMC inspect-}
    {-.| conduitPut (put :: Putter Command)-}
    {-.| mapMC inspect-}
    .| mapC show
    .| mapC B.pack
    .| appSink ad

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> argumentParser) fullDesc
  runUnixServer (serverSettings $ socketPath opts) echoC
