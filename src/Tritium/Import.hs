module Tritium.Import where

import           Control.Monad.State (StateT, liftIO)

import           SFML.System.Clock (Clock, createClock)

data Screen = MainMenu

data GameState = GameState
  { frameClock :: !Clock
  , screen     :: !Screen
  }

type GameM a = StateT GameState IO a

defaultGameState :: IO GameState
defaultGameState = do
  frameClock <- createClock
  return $ GameState frameClock MainMenu

debugP :: String -> GameM ()
debugP = liftIO . putStrLn . ((++) "[DEBUG] ")

