module IGE.Types where

import Protolude
import Data.Complex
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Lens.Micro.Platform
import Control.Concurrent.STM
import Conduit
import Graphics.UI.Gtk (KeyVal)
import Graphics.Rendering.Cairo
import qualified Data.Map.Strict as Map

-- Refactoring ideas:
-- - Use typeclass-based EditorState
-- - Use StateT based Keybinding -- use hoisting to promote pure functions
-- - 

type ℂ = Complex Double

class Translation a where
  _x :: Lens' a Double
  _y :: Lens' a Double

class Amplitwist a where
  _r :: Lens' a Double
  _θ :: Lens' a Double

instance Translation ℂ where
  _x f (x :+ y) = (:+ y) <$> f x
  _y f (x :+ y) = (x :+) <$> f x

instance Amplitwist ℂ where
  _r f z = (* z) . (:+ 0) . (/ r) <$> f r
    where r = magnitude z
  _θ f z = (* z) . cis . (+ (-θ)) <$> f θ
    where θ = phase z

class (Translation a, Amplitwist a) => RigidMotion a where
  (^.^) :: a -> a -> a
  (^*) :: a -> ℂ -> ℂ
  -- law = (a ^.^ b) ^* z = a ^* (b ^* c)

data RM = RM { rmAT :: ℂ, rmTrans :: ℂ } -- rigid motion
  deriving (Eq, Show)

makeLensesFor [("rmAT", "_at"), ("rmTrans", "_trans")] ''RM

instance Translation RM where
  _x f (RM at trans) = (RM at) <$> _x f trans
  _y f (RM at trans) = (RM at) <$> _y f trans

instance Amplitwist RM where
  _r f (RM at trans) = (flip RM trans) <$> _r f at
  _θ f (RM at trans) = (flip RM trans) <$> _θ f at

instance RigidMotion RM where
  (RM at1 trans1) ^.^ (RM at2 trans2) = RM (at1 * at2) (trans1 + (at1 * trans2))
  (RM at trans) ^* z = (z * at) + trans

class Renderable a where
  render :: a -> ℂ -> Render ()

class Renderable a => RenderNode a where
  renderNode :: a -> ℂ -> Render ()
  renderNode = render

class Renderable a => RenderEdge a where
  renderEdge :: a -> ℂ -> Render ()
  renderEdge = render

data EditorState n e = EditorState {
    esGraph :: Gr n e
  , esRM :: RM
  , esNum :: Int
  , esCommand :: [Char] -- stored backwards
  , esLabels :: [([Char], Node)]
  , esNodeMap :: Map.Map Node ℂ
  }

makeLensesFor [
    ("esGraph", "_graph")
  , ("esRM", "_rm")
  , ("esNum", "_num")
  , ("esCommand", "_cmd")
  , ("esLabels", "_labels")
  , ("esNodeMap", "_nodeMap")] ''EditorState

newtype IGEM n e a = IGEM { unIGEM :: ReaderT (TVar (EditorState n e)) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar (EditorState n e)))

runIGEM :: TVar (EditorState n e) -> IGEM n e a -> IO a
runIGEM v m = runReaderT (unIGEM m) v

type EditorM n e = State (EditorState n e)

runTVarState :: (MonadReader (TVar s) m, MonadIO m) => State s a -> m a
runTVarState action = do
  stateVar <- ask
  liftIO $ atomically $ do
    s <- readTVar stateVar
    let (a, s') = runState action s
    writeTVar stateVar s'
    return a

runTVarReader :: (MonadReader (TVar s) m, MonadIO m) => Reader s a -> m a
runTVarReader action = do
  stateVar <- ask
  s <- liftIO $ readTVarIO stateVar
  return $ runReader action s

data RefreshType = LayoutChange | NoLayoutChange
  deriving (Show, Eq)

type KeyBinding n e = ConduitM KeyVal RefreshType (IGEM n e)
