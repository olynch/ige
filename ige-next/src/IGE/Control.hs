module IGE.Control 
  ( runKeyBinding
  , basicKeyBinding
  )
  where

import Protolude hiding (empty)
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

import IGE.Types
import IGE.Keys
import IGE.Layout

basicKeyMap :: Map.Map KeyVal (EditorM ())
basicKeyMap = Map.fromList [
    (xK_plus, _rm . _at *= ((10/9) :+ 0))
  , (xK_minus, _rm . _at *= ((9/10) :+ 0))
  , (xK_h, transRel (0.5 :+ 0))
  , (xK_j, transRel (0 :+ (-0.5)))
  , (xK_k, transRel (0 :+ 0.5))
  , (xK_l, transRel ((-0.5) :+ 0))
  , (xK_greater, _rm . _at *= (cis (pi / 6)))
  , (xK_less, _rm . _at *= (cis (- pi / 6)))
  ]

transRel :: ℂ -> EditorM ()
transRel z = do
  r <- use $ _rm . _r
  _rm . _trans += (r :+ 0) * z


awaitOrFinish :: (Monad m) => a -> (i -> ConduitM i o m a) -> ConduitM i o m a
awaitOrFinish x action = await >>= maybe (return x) action

updateEditor :: EditorM () -> KeyBinding ()
updateEditor em = do
  runTVarState em
  yield NoLayoutChange

updateEditorLayout :: EditorM () -> KeyBinding ()
updateEditorLayout em = do
  runTVarState em
  yield LayoutChange

tailNonEmpty :: [a] -> [a]
tailNonEmpty (x:y:xs) = y:xs
tailNonEmpty xs = xs

getString :: [Char] -> KeyBinding (Maybe [Char])
getString init = do
  updateEditor $ _cmd .= init
  loop
  where
    handleInput kv
      | kv == xK_BackSpace = updateEditor (_cmd %= tailNonEmpty) >> loop 
      | kv == xK_Return = do
        s <- runTVarReader $ view _cmd
        updateEditor (_cmd .= "") >> return (Just $ tailDef "" $ reverse s)
      | kv == xK_Escape = updateEditor (_cmd .= "") >> return Nothing
      | otherwise = loop

    loop :: KeyBinding (Maybe [Char])
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

labelGraph :: Gr a b -> [([Char], Node)]
labelGraph gr = zip labels (nodes gr)
  where
    labels = makeLabels (length $ nodes gr) labelChars

getNode :: KeyBinding (Maybe Node)
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

addNode :: KeyBinding ()
addNode = do
  updateEditorLayout $ do 
    n <- use _num
    _graph %= insNode (n, ())
    _num += 1
      
linkNodes :: KeyBinding ()
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

deleteNode :: KeyBinding ()
deleteNode = do
  mnode <- getNode
  case mnode of
    Just n -> updateEditor $ _graph %= delNode n
    Nothing -> return ()

basicKeyBinding :: KeyBinding ()
basicKeyBinding = loop
  where
    loop = awaitOrFinish () $ \kv ->
      case Map.lookup kv basicKeyMap of
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

refresh :: (WidgetClass w) => w -> Consumer RefreshType IGEM ()
refresh widget = do
  rt <- await
  if rt == Just LayoutChange
    then runTVarState $ do
      graph <- use _graph
      _nodeMap .= layoutGr graph
    else return ()
  liftIO $ widgetQueueDraw widget
  refresh widget

runKeyBinding :: (WidgetClass w) => TBMChan KeyVal -> TVar EditorState -> w -> KeyBinding () -> IO ()
runKeyBinding keyChan editorState w kb =
  runIGEM editorState $ runConduit $
      sourceTBMChan keyChan
   .| basicKeyBinding
   .| refresh w
