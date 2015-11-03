module Main where

import           SFML.Graphics.RenderWindow (createRenderWindow)
import           SFML.Window.Types (Window)
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
  createRenderWindow vmode title style cnxst
  return ()

