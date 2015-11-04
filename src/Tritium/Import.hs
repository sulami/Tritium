module Tritium.Import where

import           Control.Monad.State (StateT, liftIO)

import           SFML.System.Time (Time)
import           SFML.System.Clock (Clock)
import           SFML.Window.Event (SFEvent (..))

type GameScreen = Time -> Maybe SFEvent -> GameM ()

data GameState = GameState
  { frameClock :: !Clock
  , screen     :: !GameScreen
  }

type GameM a = StateT GameState IO a

debugP :: String -> GameM ()
debugP = liftIO . putStrLn . ((++) "[DEBUG] ")

