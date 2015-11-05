module Main where

import           Control.Monad.State (execStateT, get, liftIO)
import           System.Exit (exitSuccess)

import           Control.Lens ((^.))
import qualified SFML.Graphics.Color as COL
import qualified SFML.Graphics.RenderWindow as RW
import           SFML.Graphics.Types (RenderWindow)
import           SFML.System.Clock (createClock, restartClock)
import           SFML.Window.Event (SFEvent (SFEvtClosed))
import           SFML.Window.VideoMode (VideoMode (..))
import           SFML.Window.Window (WindowStyle (..))

import           Tritium.Import
import           Tritium.Screen

-- | Main entry point. Sets up a window and game and then enters the core loop.
main :: IO ()
main = do
  let vmode = VideoMode 800 600 8
      title = "Tritium"
      style = [SFTitlebar, SFClose]
      cnxst = Nothing
  window <- RW.createRenderWindow vmode title style cnxst
  startingState <- defaultGameState window
  execStateT (coreLoop window) startingState
  exitSuccess

-- | The main core loop that clears and redraws the window, calls the current
-- screen function and passes the neccessary data.
coreLoop :: RenderWindow -> GameM ()
coreLoop window = do
  liftIO $ RW.clearRenderWindow window COL.blue
  event <- liftIO $ RW.pollEvent window
  state <- get
  case event of
    Just SFEvtClosed -> do
      debugP "Received window close event, terminating"
      liftIO $ do
        RW.close window
        RW.destroy window
        exitSuccess
    _                -> do
      frameTime <- liftIO . restartClock $ state^.frameClock
      -- debugP . (++ " FPS") . show . (1000000 `div`) $ frameTime
      (state^.screen) frameTime event
  liftIO . mapM_ (draw window) $ state^.drawables
  liftIO $ RW.display window
  coreLoop window

-- | The initial gamestate to use when starting.
defaultGameState :: RenderWindow -> IO GameState
defaultGameState win = do
  frameClock <- createClock
  return $ GameState frameClock mainMenu [] True win

