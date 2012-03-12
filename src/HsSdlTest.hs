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


backgroundImg = "resources/background.bmp"
spritesImg = "resources/sprites.png"
textImg = "resources/text.png"
tick = 30

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
  guyLocation :: Co2,
  liveEntities :: [Entity]
  }

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

runLoop :: GameEnv -> GameState -> IO ()
runLoop = evalStateT . runReaderT loop

sGet key = liftM key get
eGet key = liftM key ask


data Entity = Entity {
  nextState :: GameState -> Entity,
  isAlive :: Bool,
  blit :: SDL.Surface -> IO ()
  }

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

blitText :: String -> Co2 -> GameEnvM ()
blitText msg co2 = do
  t <- eGet textTiles
  s <- eGet screen
  liftIO $ T.drawTileLookup t s msg co2

drawScreen :: GameEnvM ()
drawScreen = do
  screen <- getScreen
  back <- getBackground
  fc <- sGet frameCount
  liftIO $ SDL.blitSurface back Nothing screen Nothing
  drawEntities
  blitText (show fc) (Co2 45 60)
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

loop :: GameEnvM ()
loop = do
  ents <- sGet liveEntities
  when (null ents) $ do
    guy <- makeGuy
    modify $ \s -> s { liveEntities = [guy] }
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
  runLoop gameEnv gameState

handleEvents :: GameEnvM ()
handleEvents = do
  event <- liftIO SDL.pollEvent
  case event of
    SDL.Quit -> do
      putShouldQuit True
      handleEvents
    (SDL.KeyUp _) -> do
      putShouldQuit True
      handleEvents
    SDL.NoEvent -> return ()
    _ -> handleEvents

updateGame :: GameEnvM ()
updateGame = do
  modify $ \s -> s { liveEntities = map (\e -> nextState e s) (liveEntities s) }
