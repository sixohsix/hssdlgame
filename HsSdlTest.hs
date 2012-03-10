{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}

module HsSdlTest where

import Control.Monad (when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReaderT, ReaderT, ask, MonadReader)
import Control.Monad.State (evalStateT, StateT, get, put, MonadState, modify)

import Foreign.C
import Foreign (Word32)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi


background = "background.bmp"
tick = 30

data Co2 = Co2 {
  x :: Int,
  y :: Int
  }

data GameEnv = GameEnv {
  screen :: Surface,
  back :: Surface,
  guyTile :: Tile
  }

data GameState = GameState {
  shouldQuit :: Bool,
  nextTick :: Word32,
  guyLocation :: Co2
  }

type GameStateM = StateT GameState IO
type GameEnvM = ReaderT GameEnv GameStateM

getScreen :: MonadReader GameEnv m => m Surface
getScreen = liftM screen ask

getGuyTile :: MonadReader GameEnv m => m Tile
getGuyTile = liftM guyTile ask

getNextTick :: MonadState GameState m => m Word32
getNextTick = liftM nextTick get

setNextTick :: MonadState GameState m => Word32 -> m ()
setNextTick t = modify $ \s -> s { nextTick = t }

getShouldQuit :: MonadState GameState m => m Bool
getShouldQuit = liftM shouldQuit get

setShouldQuit :: MonadState GameState m => Bool -> m ()
setShouldQuit sq = modify $ \s -> s { shouldQuit = sq }

runLoop :: GameEnv -> GameState -> IO ()
runLoop = evalStateT . runReaderT loop

initGame :: IO (GameEnv, GameState)
initGame = do
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "hello world!" []
  back <- SDL.loadBMP background
  tiles <- loadTileMap "sprites.png" 64 64
  guyTile <- return $ Tile tiles 0 0
  screen <- SDL.getVideoSurface
  curTick <- getTicks
  return (GameEnv screen back guyTile, GameState False (curTick + tick) (Co2 0 0))

drawScreen :: GameEnvM ()
drawScreen = do
  screen <- getScreen
  guyTile <- getGuyTile
  liftIO $ do
    blitTile guyTile screen 64 64
    SDL.flip screen

waitATick :: GameEnvM ()
waitATick = do
  nextTick <- getNextTick
  liftIO $ waitUntil nextTick
  setNextTick $ nextTick + tick

loop :: GameEnvM ()
loop = do
  handleEvents
  updateGame
  drawScreen
  waitATick

  shouldQuit <- getShouldQuit
  case shouldQuit of
    True -> return ()
    False -> loop

foreign export ccall my_main :: IO ()

my_main :: IO ()
my_main = SDL.withInit [InitEverything] $ do
  (gameEnv, gameState) <- initGame
  blitSurface (back gameEnv) Nothing (screen gameEnv) Nothing
  runLoop gameEnv gameState

handleEvents :: GameEnvM ()
handleEvents = do
  event <- liftIO SDL.pollEvent
  case event of
    Quit -> do
      setShouldQuit True
      handleEvents
    (KeyUp _) -> do
      setShouldQuit True
      handleEvents
    NoEvent -> return ()
    _ -> handleEvents

updateGame :: GameEnvM ()
updateGame = return ()

waitUntil :: Word32 -> IO ()
waitUntil ticks = do
  now <- getTicks
  when (now < ticks) $ do
    w <- return (ticks - now)
    --putStrLn ("Waiting " ++ (show w))
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
