{-# LANGUAGE DeriveGeneric #-}

import Messages
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Control.Monad (unless)
import Data.Semigroup ((<>))
import Network.Socket hiding (send, recv, sendTo, recvFrom)
import Network.Socket.ByteString
import Data.Conduit.Serialization.Binary
import Data.Conduit.Network.Unix
import Conduit
import Data.Char
import Numeric (showHex)

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

printBytes :: B.ByteString -> IO B.ByteString
printBytes bs = do
  putStrLn $ show $ map (flip showHex "") $ map ord $ B.unpack $ bs
  return bs

echoC :: AppDataUnix -> IO ()
echoC ad = do
  runConduit $
    appSource ad
    .| mapMC printBytes
    .| (conduitDecode :: Conduit B.ByteString IO Event)
    .| mapMC inspect
    .| mapC (\_ -> GetSelection)
    .| mapMC inspect
    .| (conduitEncode :: Conduit ControlMsg IO B.ByteString)
    .| mapMC printBytes
    .| appSink ad

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> argumentParser) fullDesc
  runUnixServer (serverSettings $ socketPath opts) echoC
