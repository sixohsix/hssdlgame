module TheGame.State where

import Foreign (Word32)

import qualified Graphics.UI.SDL as SDL
import qualified MGameLib.Tile as T
import qualified TheGame.Entity as E
import qualified MGameLib.Coordinates as C

data GameEnv = GameEnv {
  screen :: SDL.Surface,
  background :: SDL.Surface,
  guyTile :: T.Tile,
  textTiles :: [(Char, T.Tile)]
  }

data GameState = GameState {
  shouldQuit :: Bool,
  nextTick :: Word32,
  frameCount :: Int,
  guyLocation :: C.Co2,
  liveEntities :: [E.Entity GameState]
  }
