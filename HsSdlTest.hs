{-# LANGUAGE ForeignFunctionInterface #-}

module HsSdlTest where

import Foreign.C
import Foreign
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi

--import Prelude (printLn)


background = "background.bmp"
tick = 100


foreign export ccall my_main :: IO ()

my_main = SDL.withInit [InitEverything] $ do
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "hello world!" "hello world"
  back <- SDL.loadBMP background
  tiles <- loadTileMap "sprites.png" 64 64
  guyTile <- return $ Tile tiles 0 0
  screen <- SDL.getVideoSurface
  blitSurface back Nothing screen Nothing
  blitTile guyTile screen 64 64
  SDL.flip screen
  now <- getTicks
  eventLoop $ now + tick

eventLoop nextTick = SDL.waitEvent >>= checkEvent
  where checkEvent (KeyUp _) = return ()
        checkEvent _ = do updateGame
                          waitUntil nextTick
                          eventLoop $ nextTick + tick

updateGame = return ()

waitUntil ticks = do
  now <- getTicks
  wait <- return $ ticks - now
  putStrLn ("Waiting " ++ (show wait))
  delay wait


data TileMap = TileMap {
  surface :: SDL.Surface,
  tileWidth :: Int,
  tileHeight :: Int
  }

loadTileMap tileFile tileWidth tileHeight = do
  tiles <- SDLi.load tileFile
  return $ TileMap tiles tileWidth tileHeight

data Tile = Tile {
  tileMap :: TileMap,
  row :: Int,
  col :: Int
  }

tileRect tile =
  let tm = tileMap tile
      tw = tileWidth tm
      th = tileHeight tm
  in Rect ((col tile) * tw) ((row tile) * th) tw th

blitTile :: Tile -> SDL.Surface -> Int -> Int -> IO Bool
blitTile tile destSurf x y =
  let tileSurf = surface $ tileMap tile
      tileMRect = Just $ tileRect tile
      tw = tileWidth $ tileMap tile
      th = tileHeight $ tileMap tile
      destMRect = Just $ Rect x y tw th
  in blitSurface tileSurf tileMRect destSurf destMRect
