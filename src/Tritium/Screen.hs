module Tritium.Screen (
  mainMenu
) where

import           Control.Monad (when)
import           Control.Monad.State (get, modify)

import           Control.Lens ((^.))
import           SFML.Window.Event (SFEvent (..))

import           Tritium.Import
import           Tritium.UI

mainMenu :: GameScreen
mainMenu ft ev = do
  state <- get
  when (state^.setup) $ do
    Just title <- newText "MonkirtaPursuit" 46 "Tritium"
    centerText title
    addText title
    setupDone
  return ()

