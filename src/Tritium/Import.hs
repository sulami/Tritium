module Tritium.Import where

import           Control.Monad.State (StateT)

data GameState = MainMenu

type GameM a = StateT GameState IO a

