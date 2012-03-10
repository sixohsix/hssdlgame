{-# LANGUAGE ForeignFunctionInterface #-}

module HsSdlTest where

import Foreign.C
import Foreign
import Graphics.UI.SDL as SDL

background = "background.bmp"

my_main = SDL.withInit [InitEverything] $ do
  SDL.setVideoMode 640 480 24 [SDL.SWSurface]
  SDL.setCaption "hello world!" "hello world"
  back <- SDL.loadBMP background
  screen <- SDL.getVideoSurface
  blitSurface back Nothing screen Nothing
  SDL.flip screen
  eventLoop
  where eventLoop = SDL.waitEventBlocking >>= checkEvent
        checkEvent (KeyUp _) = return ()
        checkEvent _ = eventLoop


foreign export ccall my_main :: IO ()
