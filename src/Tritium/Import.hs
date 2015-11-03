module Tritium.Import where

import           Control.Monad.State (StateT, liftIO)

import           SFML.System.Clock (Clock)

data GameState = GameState
  { frameClock :: !Clock
  }

type GameM a = StateT GameState IO a

debugP :: String -> GameM ()
debugP = liftIO . putStrLn . ((++) "[DEBUG] ")

