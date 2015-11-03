module Main where

import           System.Exit (exitSuccess)

import qualified SFML.Graphics.Color as COL
import           SFML.Graphics.Types (RenderWindow)
import qualified SFML.Graphics.RenderWindow as RW
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
  coreLoop window

coreLoop :: RenderWindow -> IO ()
coreLoop window = do
  RW.clearRenderWindow window COL.black
  event <- RW.pollEvent window
  case event of
    Just ev -> handleEvent window ev
    Nothing -> return ()
  RW.display window
  coreLoop window

handleEvent :: RenderWindow -> SFEvent -> IO ()
handleEvent window event = do
  case event of
    SFEvtClosed -> do RW.close window
                      RW.destroy window
                      exitSuccess
    _           -> return ()

