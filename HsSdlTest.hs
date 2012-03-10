{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}

module HsSdlTest where

import System.Random (randomIO)

import Control.Monad (when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReaderT, ReaderT, ask, MonadReader)
import Control.Monad.State (evalStateT, StateT, get, put, MonadState, modify)

import Foreign.C
import Foreign (Word32)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi


backgroundImg = "background.bmp"
tick = 30

data Co2 = Co2 {
  x :: Int,
  y :: Int
  }

data GameEnv = GameEnv {
  screen :: SDL.Surface,
  background :: SDL.Surface,
  guyTile :: Tile
  }

data GameState = GameState {
  shouldQuit :: Bool,
  nextTick :: Word32,
  frameCount :: Int,
  guyLocation :: Co2
  }

type GameStateM = StateT GameState IO
type GameEnvM = ReaderT GameEnv GameStateM

getScreen :: MonadReader GameEnv m => m SDL.Surface
getScreen = liftM screen ask

getBackground :: MonadReader GameEnv m => m SDL.Surface
getBackground = liftM background ask

getGuyTile :: MonadReader GameEnv m => m Tile
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

initGame :: IO (GameEnv, GameState)
initGame = do
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "hello world!" []
  back <- SDL.loadBMP backgroundImg
  tiles <- loadTileMap "sprites.png" 64 64
  guyTile <- return $ Tile tiles 0 0
  screen <- SDL.getVideoSurface
  curTick <- getTicks
  return (GameEnv screen back guyTile, GameState False (curTick + tick) 0 (Co2 0 0))

drawScreen :: GameEnvM ()
drawScreen = do
  screen <- getScreen
  back <- getBackground
  guyTile <- getGuyTile
  guyLoc <- getGuyLocation
  liftIO $ do
    SDL.blitSurface back Nothing screen Nothing
    blitTile guyTile screen (x guyLoc) (y guyLoc)
    SDL.flip screen

waitATick :: GameEnvM ()
waitATick = do
  nextTick <- getNextTick
  liftIO $ waitUntil nextTick
  putNextTick $ nextTick + tick
  where
    waitUntil :: Word32 -> IO ()
    waitUntil ticks = do
      now <- SDL.getTicks
      when (now < ticks) $ do
        w <- return (ticks - now)
        --putStrLn ("Waiting " ++ (show w))
        delay w

loop :: GameEnvM ()
loop = do
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
my_main = SDL.withInit [InitEverything] $ do
  (gameEnv, gameState) <- initGame
  runLoop gameEnv gameState

handleEvents :: GameEnvM ()
handleEvents = do
  event <- liftIO SDL.pollEvent
  case event of
    Quit -> do
      putShouldQuit True
      handleEvents
    (KeyUp _) -> do
      putShouldQuit True
      handleEvents
    NoEvent -> return ()
    _ -> handleEvents

updateGame :: GameEnvM ()
updateGame = do
  frameCount <- getFrameCount
  when (0 == (mod frameCount 30)) $ let
    randomWithin :: Int -> GameEnvM Int
    randomWithin maxV = do
      r <- liftIO randomIO
      return $ mod r maxV
    in do
      rX <- randomWithin 640
      rY <- randomWithin 480
      putGuyLocation $ Co2 rX rY

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
