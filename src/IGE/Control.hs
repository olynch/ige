module IGE.Control 
  ( runKeyBinding
  , basicKeyBinding
  )
  where

import Prelude (String)
import Protolude hiding (empty, readFile, writeFile)
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import Conduit
import Data.Conduit.TMChan
import Graphics.UI.Gtk hiding (get)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Concurrent.STM
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>), viewl, ViewL(..), Seq(..))
import Control.Monad.Trans.Maybe
import Data.Text (pack)
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString.Lazy (readFile, writeFile)
import System.Directory (doesFileExist)

import IGE.Types
import IGE.Keys
import IGE.Layout
import IGE.Serialization
import IGE.Render

instance Inputable Node where
  readInput = MaybeT (initLabels >> loop)
    where
      initLabels = updateEditor $ do
        gr <- use _graph
        _labels .= labelGraph gr
      reset = updateEditor $ _labels .= []
      loop = awaitOrFinish Nothing $ \kv ->
        case keyToChar kv of
          (Just c) -> do
            labels <- runTVarReader $ view _labels
            let labels' = [(xs, node) | (x:xs, node) <- labels, x == c]
            case labels' of
              [] -> reset >> return Nothing
              [(_, node)] -> reset >> return (Just node)
              xs -> do
                updateEditor $ _labels .= labels'
                loop
          Nothing -> reset >> return Nothing

labelChars = "asdfghjkl"

makeLabels :: Int -> String -> [String]
makeLabels n alphabet = helper n alphabet "" Seq.empty
  where
    helper :: Int -> String -> String -> Seq String -> [String]
    helper 0 _ _ queue = reverse <$> toList queue
    helper n (c:cs) postfix queue =
      helper (n - 1) cs postfix (queue |> (c:postfix))
    helper n [] _ queue =
      case viewl queue of
        a :< rest -> helper (n + 1) alphabet a rest
        EmptyL -> []

labelGraph :: Gr n e -> [(String, Node)]
labelGraph gr = zip labels (nodes gr)
  where
    labels = makeLabels (length $ nodes gr) labelChars

instance Inputable String where
  readInput = MaybeT $ do
    loop
    where
      handleInput kv
        | kv == xK_BackSpace = updateEditor (_cmd %= tailDef []) >> loop 
        | kv == xK_Return = do
          s <- runTVarReader $ view _cmd
          updateEditor (_cmd .= "") >> return (Just $ reverse s)
        | kv == xK_Escape = updateEditor (_cmd .= "") >> return Nothing
        | otherwise = loop

      loop = awaitOrFinish Nothing $ \kv -> 
        case keyToChar kv of
          (Just c) -> updateEditor (_cmd %= (c:)) >> loop
          Nothing ->
            handleInput kv


instance Inputable () where
  readInput = return ()
  readInputPrompt _ = readInput

instance Inputable Text where
  readInput = pack <$> readInput

instance NodeType Text where

instance EdgeType Text where

instance NodeType () where

instance EdgeType () where

-- to do: refactor these into pure code, and make a new type for "action that could fail"

addNode :: (Inputable n) => MaybeT (KeyBinding n e) ()
addNode = do
  label <- readInputPrompt "label: "
  lift $ updateEditorLayout $ do 
    n <- use _num
    _graph %= insNode (n, label)
    _num += 1
      
linkNodes :: (Inputable e) => MaybeT (KeyBinding n e) ()
linkNodes = do
  n1 <- readInputPrompt "SELECT NODE 1"
  n2 <- readInputPrompt "SELECT NODE 2"
  label <- readInputPrompt "label: "
  lift $ updateEditorLayout $ _graph %= insEdge (n1, n2, label)

deleteNode :: MaybeT (KeyBinding n e) ()
deleteNode = do
  node <- readInput
  lift $ updateEditor $ _graph %= delNode node

readGraph :: (FromJSON n, FromJSON e) => MaybeT (KeyBinding n e) ()
readGraph = do
  filename <- readInputPrompt "filename: "
  liftIO (doesFileExist filename) >>= guard
  newgraph <- MaybeT (graphFromBS <$> liftIO (readFile filename))
  lift $ updateEditorLayout $ _graph .= newgraph
  
writeGraph :: (ToJSON n, ToJSON e) => MaybeT (KeyBinding n e) ()
writeGraph = do
  fn <- readInputPrompt "filename: "
  g <- lift $ runTVarReader $ view _graph
  liftIO $ writeFile fn $ graphToBS g

transRel :: ℂ -> EditorM n e ()
transRel z = do
  r <- use $ _rm . _r
  _rm . _trans += (r :+ 0) * z

navigationKeyMap :: Map.Map KeyVal (EditorM n e ())
navigationKeyMap = Map.fromList [
    (xK_plus, _rm . _at *= ((10/9) :+ 0))
  , (xK_minus, _rm . _at *= ((9/10) :+ 0))
  , (xK_h, transRel (0.5 :+ 0))
  , (xK_j, transRel (0 :+ (-0.5)))
  , (xK_k, transRel (0 :+ 0.5))
  , (xK_l, transRel ((-0.5) :+ 0))
  , (xK_greater, _rm . _at *= cis (pi / 6))
  , (xK_less, _rm . _at *= cis ((- pi) / 6))
  ]

basicKeyBinding :: (NodeType n, EdgeType e) => KeyBinding n e ()
basicKeyBinding = loop
  where
    loop = awaitOrFinish () $ \kv ->
      case Map.lookup kv navigationKeyMap of
        (Just em) -> do
          updateEditor em
          loop
        Nothing -> handleSpecial kv
    handleSpecial kv
      | kv == xK_colon = do
        runMaybeT $ do
          cmd <- readInputPrompt ":"
          putText cmd
        loop
      | kv == xK_a = runMaybeT addNode >> loop
      | kv == xK_c = runMaybeT linkNodes >> loop
      | kv == xK_d = runMaybeT deleteNode >> loop
      | kv == xK_w = runMaybeT writeGraph >> loop
      | kv == xK_o = runMaybeT readGraph >> loop
      | kv == xK_q = liftIO mainQuit >> return ()
      | otherwise = loop

refresh :: (WidgetClass w) => w -> Consumer RefreshType (IGEM n e) ()
refresh widget = do
  rt <- await
  if rt == Just LayoutChange
    then runTVarState $ do
      graph <- use _graph
      _nodeMap .= layoutGr graph
    else return ()
  liftIO $ widgetQueueDraw widget
  refresh widget

runKeyBinding ::
  (WidgetClass w, NodeType n, EdgeType e) => 
  TBMChan KeyVal
  -> TVar (EditorState n e)
  -> w
  -> KeyBinding n e ()
  -> IO ()
runKeyBinding keyChan editorState w kb =
  runIGEM editorState $ runConduit $
      sourceTBMChan keyChan
   .| kb
   .| refresh w
