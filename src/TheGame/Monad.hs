{-# OPTIONS_GHC -XFlexibleContexts #-}

module TheGame.Monad where

import Control.Monad (liftM)
import Control.Monad.Reader (runReaderT, ReaderT, ask, MonadReader)
import Control.Monad.State (evalStateT, StateT, get, put, MonadState, modify)
import Foreign (Word32)

import qualified Graphics.UI.SDL as SDL

import qualified MGameLib.Tile as T
import MGameLib.Coordinates (Co2 (Co2, x, y))

import qualified TheGame.Entity as E
import TheGame.State

type GameStateM = StateT GameState IO
type GameEnvM = ReaderT GameEnv GameStateM

getScreen :: MonadReader GameEnv m => m SDL.Surface
getScreen = liftM screen ask

getBackground :: MonadReader GameEnv m => m SDL.Surface
getBackground = liftM background ask

getGuyTile :: MonadReader GameEnv m => m T.Tile
getGuyTile = liftM guyTile ask

getNextTick :: MonadState GameState m => m Word32
getNextTick = liftM nextTick get

putNextTick :: MonadState GameState m => Word32 -> m ()
putNextTick t = modify $ \s -> s { nextTick = t }

getFrameCount :: MonadState GameState m => m Int
getFrameCount = liftM frameCount get

incrementFrameCount :: MonadState GameState m => m ()
incrementFrameCount = modify $ \s -> s { frameCount = (frameCount s) + 1 }

getShouldQuit :: MonadState GameState m => m Bool
getShouldQuit = liftM shouldQuit get

putShouldQuit :: MonadState GameState m => Bool -> m ()
putShouldQuit sq = modify $ \s -> s { shouldQuit = sq }

getGuyLocation :: MonadState GameState m => m Co2
getGuyLocation = liftM guyLocation get

putGuyLocation :: MonadState GameState m => Co2 -> m ()
putGuyLocation guyLoc = modify $ \s -> s { guyLocation = guyLoc }

sGet key = liftM key get
eGet key = liftM key ask
