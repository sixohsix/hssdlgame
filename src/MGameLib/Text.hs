module MGameLib.Text where

import qualified Graphics.UI.SDL as SDL

import MGameLib.Tile (loadTileMapMap, Tile)


loadTextTiles :: String -> IO [(Char, Tile)]
loadTextTiles textImg =
  loadTileMapMap textImg 16 32
    ([(' ', (0, 0))] ++ map (\v -> (show v !! 0, (0, v + 1))) [0..9])
