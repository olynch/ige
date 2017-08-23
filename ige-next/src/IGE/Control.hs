module IGE.Control 
  ( runKeyBinding
  , basicKeyBinding
  , textKeyBinding
  )
  where

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

navigationKeyMap :: Map.Map KeyVal (EditorM n e ())
navigationKeyMap = Map.fromList [
    (xK_plus, _rm . _at *= ((10/9) :+ 0))
  , (xK_minus, _rm . _at *= ((9/10) :+ 0))
  , (xK_h, transRel (0.5 :+ 0))
  , (xK_j, transRel (0 :+ (-0.5)))
  , (xK_k, transRel (0 :+ 0.5))
  , (xK_l, transRel ((-0.5) :+ 0))
  , (xK_greater, _rm . _at *= (cis (pi / 6)))
  , (xK_less, _rm . _at *= (cis (- pi / 6)))
  ]

transRel :: ℂ -> EditorM n e ()
transRel z = do
  r <- use $ _rm . _r
  _rm . _trans += (r :+ 0) * z


awaitOrFinish :: (Monad m) => a -> (i -> ConduitM i o m a) -> ConduitM i o m a
awaitOrFinish x action = await >>= maybe (return x) action

updateEditor :: EditorM n e a -> KeyBinding n e a
updateEditor em = do
  x <- runTVarState em
  yield NoLayoutChange
  return x

updateEditorLayout :: EditorM n e a -> KeyBinding n e a
updateEditorLayout em = do
  x <- runTVarState em
  yield LayoutChange
  return x

tailButN :: Int -> [a] -> [a]
tailButN n xs = if length theTail < n then xs else theTail
  where theTail = tailDef [] xs

getString :: [Char] -> KeyBinding n e (Maybe [Char])
getString init' = do
  updateEditor $ _cmd .= init
  loop
  where
    init = reverse init'
    n = length init
    handleInput kv
      | kv == xK_BackSpace = updateEditor (_cmd %= tailButN n) >> loop 
      | kv == xK_Return = do
        s <- runTVarReader $ view _cmd
        updateEditor (_cmd .= "") >> return (Just $ drop n $ reverse s)
      | kv == xK_Escape = updateEditor (_cmd .= "") >> return Nothing
      | otherwise = loop

    loop = awaitOrFinish Nothing $ \kv -> 
      case keyToChar kv of
        (Just c) -> updateEditor (_cmd %= (c:)) >> loop
        Nothing ->
          handleInput kv

labelChars = "asdfghjkl"

makeLabels :: Int -> [Char] -> [[Char]]
makeLabels n alphabet = helper n alphabet "" Seq.empty
  where
    helper :: Int -> [Char] -> [Char] -> Seq [Char] -> [[Char]]
    helper 0 _ _ queue = reverse <$> toList queue
    helper n (c:cs) postfix queue =
      helper (n - 1) cs postfix (queue |> (c:postfix))
    helper n [] _ queue =
      case viewl queue of
        a :< rest -> helper (n + 1) alphabet a rest
        EmptyL -> []

labelGraph :: Gr n e -> [([Char], Node)]
labelGraph gr = zip labels (nodes gr)
  where
    labels = makeLabels (length $ nodes gr) labelChars

getNode :: KeyBinding n e (Maybe Node)
getNode = initLabels >> loop
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

addNode :: KeyBinding () e ()
addNode = do
  updateEditorLayout $ do 
    n <- use _num
    _graph %= insNode (n, ())
    _num += 1

addNodeText :: KeyBinding Text e ()
addNodeText = do
  mlabel <- getString "label: "
  case mlabel of
    Just label -> updateEditorLayout $ do
      n <- use _num
      _graph %= insNode (n, pack label)
      _num += 1
    Nothing -> return ()
      
linkNodes :: KeyBinding n () ()
linkNodes = do
  mnodes <- runMaybeT $ do
    lift $ updateEditor $ _cmd .= reverse "SELECT NODE 1"
    n1 <- MaybeT getNode
    lift $ updateEditor $ _cmd .= reverse "SELECT NODE 2"
    n2 <- MaybeT getNode
    return (n1, n2)
  updateEditor $ _cmd .= ""
  case mnodes of
    Just (n1, n2) ->
      updateEditorLayout $ _graph %= insEdge (n1, n2, ())
    Nothing ->
      return ()

linkNodesText :: KeyBinding n Text ()
linkNodesText = do
  mnodes <- runMaybeT $ do
    lift $ updateEditor $ _cmd .= reverse "SELECT NODE 1"
    n1 <- MaybeT getNode
    lift $ updateEditor $ _cmd .= reverse "SELECT NODE 2"
    n2 <- MaybeT getNode
    label <- MaybeT $ getString "label: "
    return (n1, n2, label)
  case mnodes of
    Just (n1, n2, label) ->
      updateEditorLayout $ _graph %= insEdge (n1, n2, pack label)
    Nothing ->
      return ()

deleteNode :: KeyBinding n e ()
deleteNode = do
  mnode <- getNode
  case mnode of
    Just n -> updateEditor $ _graph %= delNode n
    Nothing -> return ()

readGraph :: (FromJSON n, FromJSON e) => KeyBinding n e ()
readGraph = do
  mnewgraph <- runMaybeT $ do
    filename <- MaybeT $ getString "filename: "
    liftIO (doesFileExist filename) >>= guard
    MaybeT $ graphFromBS <$> liftIO (readFile filename)
  case mnewgraph of
    Just newgraph ->
      updateEditorLayout $ _graph .= newgraph
    Nothing -> return ()
  
writeGraph :: (ToJSON n, ToJSON e) => KeyBinding n e ()
writeGraph = do
  mfilename <- getString "filename: "
  case mfilename of
    Just fn -> do
      g <- runTVarReader $ view _graph
      liftIO $ writeFile fn $ graphToBS g
    Nothing -> return ()

textKeyBinding :: KeyBinding Text Text ()
textKeyBinding = loop
  where
    loop = awaitOrFinish () $ \kv ->
      case Map.lookup kv navigationKeyMap of
        (Just em) -> do
          updateEditor em
          loop
        Nothing -> handleSpecial kv
    handleSpecial kv
      | kv == xK_colon = do
        cmd <- getString ":"
        print cmd
        loop
      | kv == xK_a = addNodeText >> loop
      | kv == xK_c = linkNodesText >> loop
      | kv == xK_d = deleteNode >> loop
      | kv == xK_w = writeGraph >> loop
      | kv == xK_o = readGraph >> loop
      | kv == xK_q = liftIO mainQuit >> return ()
      | otherwise = loop

basicKeyBinding :: KeyBinding () () ()
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
        cmd <- getString ":"
        print cmd
        loop
      | kv == xK_a = addNode >> loop
      | kv == xK_c = linkNodes >> loop
      | kv == xK_d = deleteNode >> loop
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

runKeyBinding :: (WidgetClass w, RenderNode n, RenderEdge e) => TBMChan KeyVal -> TVar (EditorState n e) -> w -> KeyBinding n e () -> IO ()
runKeyBinding keyChan editorState w kb =
  runIGEM editorState $ runConduit $
      sourceTBMChan keyChan
   .| kb
   .| refresh w
