module Tritium.Import where

import           Control.Monad.State (StateT, liftIO)

import           SFML.System.Time (Time)
import           SFML.System.Clock (Clock, createClock)
import           SFML.Window.Event (SFEvent (..))

type GameStep = Time -> Maybe SFEvent -> GameM ()

data GameState = GameState
  { frameClock :: !Clock
  , screen     :: !GameStep
  }

type GameM a = StateT GameState IO a

defaultGameState :: IO GameState
defaultGameState = do
  frameClock <- createClock
  return $ GameState frameClock (\t e -> return ())

debugP :: String -> GameM ()
debugP = liftIO . putStrLn . ((++) "[DEBUG] ")

