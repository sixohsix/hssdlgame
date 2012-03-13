module TheGame.Entity where

import qualified Graphics.UI.SDL as SDL

data Entity gameState = Entity {
  nextState :: gameState -> Entity gameState,
  isAlive :: Bool,
  blit :: SDL.Surface -> IO ()
  }
