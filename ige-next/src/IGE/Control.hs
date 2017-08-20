module IGE.Control 
  ( runKeyBinding
  , basicKeyBinding
  )
  where

import Protolude
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import Conduit
import Data.Conduit.TMChan
import Graphics.UI.Gtk hiding (get)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Concurrent.STM

import IGE.Types

basicKeyMap :: Map.Map Char (EditorM ())
basicKeyMap = Map.fromList [
    ('+', _rm . _at *= ((10/9) :+ 0))
  , ('-', _rm . _at *= ((9/10) :+ 0))
  , ('h', transRel (0.5 :+ 0))
  , ('j', transRel (0 :+ (-0.5)))
  , ('k', transRel (0 :+ 0.5))
  , ('l', transRel ((-0.5) :+ 0))
  , ('<', _rm . _at *= (cis (pi / 6)))
  , ('>', _rm . _at *= (cis (- pi / 6)))
  , ('a', addNode)
  ]

transRel :: ℂ -> EditorM ()
transRel z = do
  r <- use $ _rm . _r
  _rm . _trans += (r :+ 0) * z

addNode :: EditorM ()
addNode = do
  n <- use _num
  _graph %= insNode (n, ())
  _num += 1

awaitOrFinish :: (Monad m) => (i -> ConduitM i o m ()) -> ConduitM i o m ()
awaitOrFinish action = await >>= maybe (return ()) action

basicKeyBinding :: KeyBinding ()
basicKeyBinding = loop
  where
    loop = awaitOrFinish $ \kv ->
      case keyToChar kv >>= (`Map.lookup` basicKeyMap) of
        (Just em) -> do
          lift $ runTVarState em
          yield ()
          loop
        Nothing -> loop

refresh :: (WidgetClass w) => w -> Consumer () IGEM ()
refresh widget = do
  _ <- await
  liftIO $ widgetQueueDraw widget
  refresh widget

runKeyBinding :: (WidgetClass w) => TBMChan KeyVal -> TVar EditorState -> w -> KeyBinding () -> IO ()
runKeyBinding keyChan editorState w kb =
  runIGEM editorState $ runConduit $
      sourceTBMChan keyChan
   .| basicKeyBinding
   .| refresh w
