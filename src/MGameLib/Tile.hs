module MGameLib.Tile where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi

import MGameLib.Coordinates (Co2 (Co2))


data TileMap = TileMap {
  surface :: SDL.Surface,
  tileWidth :: Int,
  tileHeight :: Int
  }

data Tile = Tile {
  tileMap :: TileMap,
  row :: Int,
  col :: Int
  }

loadTileMap tileFile tileWidth tileHeight = do
  tiles <- SDLi.load tileFile
  return $ TileMap tiles tileWidth tileHeight

tileRect tile =
  let tm = tileMap tile
      tw = tileWidth tm
      th = tileHeight tm
  in SDL.Rect ((col tile) * tw) ((row tile) * th) tw th

blitTile :: Tile -> SDL.Surface -> Int -> Int -> IO Bool
blitTile tile destSurf x y =
  let tileSurf = surface $ tileMap tile
      tileMRect = Just $ tileRect tile
      tw = tileWidth $ tileMap tile
      th = tileHeight $ tileMap tile
      destMRect = Just $ SDL.Rect x y tw th
  in SDL.blitSurface tileSurf tileMRect destSurf destMRect

loadTileMapMap :: Eq k => String -> Int -> Int -> [(k, (Int, Int))] -> IO [(k, Tile)]
loadTileMapMap tileFile tw th tileSpecs = do
  tmap <- loadTileMap tileFile tw th
  return $ map (\(key, (r, c)) -> (key, Tile tmap r c)) tileSpecs

drawTileLookup :: Eq k => [(k, Tile)] -> SDL.Surface -> [k] -> Co2 -> IO ()
drawTileLookup tileSet surf str (Co2 x y) = do
  mapM drawOne (zip str [0..])
  return ()
  where drawOne (c, i) =
          let Just tile = lookup c tileSet
              tw = tileWidth $ tileMap tile
          in blitTile tile surf (x + (i * tw)) y
