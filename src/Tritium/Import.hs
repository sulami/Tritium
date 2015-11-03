module Tritium.Import where

import           Control.Monad.State (StateT, liftIO)

data GameState = MainMenu

type GameM a = StateT GameState IO a

debugP :: String -> GameM ()
debugP = liftIO . putStrLn . ((++) "[DEBUG] ")

