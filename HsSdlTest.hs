{-# LANGUAGE ForeignFunctionInterface #-}

module HsSdlTest where

import Control.Monad (when)

import Foreign.C
import Foreign
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi


background = "background.bmp"
tick = 30


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
  eventLoop 0

eventLoop :: Word32 -> IO ()
eventLoop nextTick = SDL.pollEvent >>= checkEvent
  where checkEvent (KeyUp _) = return ()
        checkEvent _ = do updateGame
                          waitUntil nextTick
                          eventLoop $ nextTick + tick

updateGame :: IO ()
updateGame = return ()

waitUntil :: Word32 -> IO ()
waitUntil ticks = do
  now <- getTicks
  when (now < ticks) $ do
    w <- return (ticks - now)
    putStrLn ("Waiting " ++ (show w))
    delay w


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
