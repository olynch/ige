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
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map
import System.Console.Haskeline
import Data.Conduit.TMChan

data Connection = Connection { socketPath :: FilePath }

argumentParser :: Parser Connection
argumentParser = Connection
  <$> argument str
      ( metavar "SOCKETPATH"
     <> help "path to socket")

inspect :: (Show a) => a -> IO a
inspect a = do
  print a
  return a

doMaybe :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
doMaybe = maybe (return ())

type IgeC = ConduitM Event ControlMsg IO

getSelection :: MaybeT IgeC Int
getSelection = MaybeT $ do
  yield $ DispMsg GetSelection
  mmsg <- await
  return $ do
    msg <- mmsg
    let (Selection ni) = msg
    return ni

addNode :: IgeC ()
addNode = yield $ GraphMsg AddNode

delNode :: IgeC ()
delNode = do
  mni <- runMaybeT getSelection
  doMaybe (yield . GraphMsg . DelNode) mni

addEdge :: IgeC ()
addEdge = do
  ids <- runMaybeT $ do
    ni1 <- getSelection
    ni2 <- getSelection
    return (ni1, ni2)
  doMaybe (yield . GraphMsg . uncurry AddEdge) ids

repl :: TBMChan Event -> IO (String -> IO ())
repl = do
  minput <- getInputLine "> "
  case minput of
     Nothing -> return ()
     Just ":q" -> return ()
     Just input -> do
       outputStrLn $ "Input was: " ++ input
       loop

keybindings :: Map.Map Int (IgeC ())
keybindings = Map.fromList [(ord 'a', addNode), (ord 'd', delNode), (ord 'e', addEdge)]

getBinding :: Int -> Map.Map Int (IgeC ()) -> IgeC ()
getBinding k kb = do
  doMaybe id $ Map.lookup k kb

mainC :: ConduitM Event ControlMsg IO ()
mainC = do
    mev <- await
    flip doMaybe mev (\ev -> case ev of
      (KeyPress k _) -> getBinding k keybindings
      _ -> return ())
    mainC

printBytes :: B.ByteString -> IO B.ByteString
printBytes bs = do
  putStrLn $ show $ map (flip showHex "") $ map ord $ B.unpack $ bs
  return bs

runServerLog :: AppDataUnix -> IO ()
runServerLog ad = do
  runConduit $
    appSource ad
    .| mapMC printBytes
    .| (conduitDecode :: Conduit B.ByteString IO Event)
    .| mapMC inspect
    .| mainC
    .| mapMC inspect
    .| (conduitEncode :: Conduit ControlMsg IO B.ByteString)
    .| mapMC printBytes
    .| appSink ad

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> argumentParser) fullDesc
  runUnixServer (serverSettings $ socketPath opts) runServerLog
