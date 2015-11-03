module Main where

import           Control.Monad.State (execStateT, get, liftIO)
import           System.Exit (exitSuccess)

import qualified SFML.Graphics.Color as COL
import qualified SFML.Graphics.RenderWindow as RW
import           SFML.Graphics.Types (RenderWindow)
import           SFML.System.Clock (restartClock)
import           SFML.Window.Event (SFEvent (..))
import           SFML.Window.VideoMode (VideoMode (..))
import           SFML.Window.Window (WindowStyle (..))

import           Tritium.Import

main :: IO ()
main = do
  let vmode = VideoMode 800 600 8
      title = "Tritium"
      style = [SFTitlebar, SFClose]
      cnxst = Nothing
  window <- RW.createRenderWindow vmode title style cnxst
  startingState <- defaultGameState
  execStateT (coreLoop window) startingState
  exitSuccess

coreLoop :: RenderWindow -> GameM ()
coreLoop window = do
  liftIO $ RW.clearRenderWindow window COL.black
  event <- liftIO $ RW.pollEvent window
  case event of
    Just SFEvtClosed -> do
      debugP "Received window close event, terminating"
      liftIO $ do
        RW.close window
        RW.destroy window
        exitSuccess
    _                -> do
      state <- get
      frameTime <- liftIO . restartClock $ frameClock state
      -- debugP . (++ " FPS") . show . (1000000 `div`) $ frameTime
      screen state frameTime event
  liftIO $ RW.display window
  coreLoop window

