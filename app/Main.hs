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
import           Tritium.UI

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
    Just ev -> handleEvent window ev
    Nothing -> return ()
  step window
  liftIO $ RW.display window
  coreLoop window

handleEvent :: RenderWindow -> SFEvent -> GameM ()
handleEvent window event = do
  case event of
    SFEvtClosed -> do debugP "Close event"
                      liftIO $ RW.close window
                      liftIO $ RW.destroy window
                      liftIO $ exitSuccess
    _           -> return ()

step :: RenderWindow -> GameM ()
step window = do
  state <- get
  frameTime <- liftIO . restartClock $ frameClock state
  return ()

