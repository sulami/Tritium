module Main where

import           Graphics.UI.Fungen

import           Tritium.Import
import           Tritium.Menu

main :: IO ()
main = do
  let windowConfig = ((0,0), (800,600), "Tritium v0.1")
      gameMap = colorMap 0.0 0.0 0.0 250 250
  funInit windowConfig gameMap [] MainMenu () [] coreLoop Idle []

coreLoop :: TritiumAction ()
coreLoop = do
  gState <- getGameState
  case gState of
    MainMenu -> mainMenu

