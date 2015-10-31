module Main where

import           Graphics.UI.Fungen (RefreshType(Idle), colorMap, funInit)

main :: IO ()
main = do
  let windowConfig = ((0,0), (800,600), "Tritium v0.1")
      gameMap = colorMap 0.0 0.0 0.0 250 250
  funInit windowConfig gameMap [] () () [] (return ()) Idle []

