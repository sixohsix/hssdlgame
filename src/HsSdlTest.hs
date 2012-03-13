{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XMultiParamTypeClasses #-}


module HsSdlTest where

import qualified System.Random as Random

import Control.Monad (when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReaderT, ReaderT, ask, MonadReader)
import Control.Monad.State (evalStateT, StateT, get, put, MonadState, modify)

import Foreign (Word32)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi

import qualified MGameLib.Tile as T
import MGameLib.Text (loadTextTiles)
import MGameLib.Coordinates (Co2 (Co2, x, y))

import TheGame.Monad
import TheGame.Entity


backgroundImg = "resources/background.bmp"
spritesImg = "resources/sprites.png"
textImg = "resources/text.png"
tick = 30


makeGuy :: GameEnvM Entity
makeGuy = do
  randomGen <- liftIO $ Random.newStdGen
  gTile <- eGet guyTile
  return $ guy gTile randomGen (Co2 40 40)

guy :: T.Tile -> Random.StdGen -> Co2 -> Entity
guy tile randomGen loc = Entity nextState isAlive blit where
  unchanged = guy tile randomGen loc
  nextState = \gameState ->
    if 0 == (mod (frameCount gameState) 30)
    then let randomWithin :: Int -> Random.StdGen -> (Int, Random.StdGen)
             randomWithin maxV = Random.randomR (0, maxV)
             (rX, g0) = randomWithin 640 randomGen
             (rY, g1) = randomWithin 480 g0
         in guy tile g1 (Co2 rX rY)
    else unchanged
  isAlive = True
  blit = \surf -> do
    T.blitTile tile surf (x loc) (y loc)
    return ()

makeScore :: GameEnvM Entity
makeScore = do
  t <- eGet textTiles
  let blitText = T.drawTileLookup t
    in return $ score blitText 0

score :: (SDL.Surface -> String -> Co2 -> IO ()) -> Int -> Entity
score blitText s = Entity nextState isAlive blit where
  unchanged = score blitText s
  nextState = \state -> score blitText (s + 1)
  isAlive = True
  blit = \surf -> blitText surf (show s) (Co2 45 60)


initGame :: IO (GameEnv, GameState)
initGame = do
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "hello world!" []
  back <- SDL.loadBMP backgroundImg
  tiles <- T.loadTileMap spritesImg 64 64
  guyTile <- return $ T.Tile tiles 0 0
  textTiles <- loadTextTiles textImg
  screen <- SDL.getVideoSurface
  curTick <- SDL.getTicks
  return (GameEnv {
             screen = screen,
             background = back,
             guyTile = guyTile,
             textTiles = textTiles
             },
          GameState {
            shouldQuit = False,
            nextTick = (curTick + tick),
            frameCount = 0,
            guyLocation = Co2 0 0,
            liveEntities = []
            }
         )

drawScreen :: GameEnvM ()
drawScreen = do
  screen <- getScreen
  back <- getBackground
  liftIO $ SDL.blitSurface back Nothing screen Nothing
  drawEntities
  liftIO $ SDL.flip screen

drawEntities :: GameEnvM ()
drawEntities = do
  screen <- eGet screen
  ents <- sGet liveEntities
  liftIO $ mapM (\e -> blit e screen) ents
  return ()

waitATick :: GameEnvM ()
waitATick = do
  nextTick <- getNextTick
  waitUntil nextTick
  putNextTick $ nextTick + tick
  where
    waitUntil :: Word32 -> GameEnvM ()
    waitUntil ticks = liftIO $ do
      now <- SDL.getTicks
      when (now < ticks) $ do
        w <- return (ticks - now)
        --putStrLn ("Waiting " ++ (show w))
        SDL.delay w

runLoop :: GameEnv -> GameState -> IO ()
runLoop = evalStateT . runReaderT

loop :: GameEnvM ()
loop = do
  ents <- sGet liveEntities
  when (null ents) $ do
    guy <- makeGuy
    score <- makeScore
    modify $ \s -> s { liveEntities = [guy, score] }
  handleEvents
  updateGame
  drawScreen
  incrementFrameCount
  waitATick

  shouldQuit <- getShouldQuit
  case shouldQuit of
    True -> return ()
    False -> loop

foreign export ccall my_main :: IO ()
my_main = SDL.withInit [SDL.InitEverything] $ do
  (gameEnv, gameState) <- initGame
  runLoop gameEnv gameState loop

handleEvents :: GameEnvM ()
handleEvents = do
  event <- liftIO SDL.pollEvent
  case event of
    SDL.Quit -> do
      putShouldQuit True
    SDL.KeyUp _ -> do
      putShouldQuit True
    _ -> return ()
  case event of
    SDL.NoEvent -> return ()
    _ -> handleEvents

updateGame :: GameEnvM ()
updateGame =
  modify $ \s -> s { liveEntities = map (\e -> nextState e s) (liveEntities s) }
