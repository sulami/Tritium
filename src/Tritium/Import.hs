{-# LANGUAGE TemplateHaskell #-}

module Tritium.Import where

import           Control.Monad.State (StateT, liftIO, modify)
import           System.Exit (exitFailure)

import           Control.Lens (makeLenses, set)
import qualified SFML.Graphics.RenderWindow as RW
import qualified SFML.Graphics.Types as GT
import           SFML.System.Time (Time)
import           SFML.System.Clock (Clock)
import           SFML.Window.Event (SFEvent (..))

type GameScreen = Time -> Maybe SFEvent -> GameM ()

data Drawable = DShape GT.Shape
              | DSprite GT.Sprite
              | DText GT.Text

draw :: GT.RenderWindow -> Drawable -> IO ()
draw win (DShape s)  = RW.drawShape win s Nothing
draw win (DSprite s) = RW.drawSprite win s Nothing
draw win (DText t)   = RW.drawText win t Nothing

data GameState = GameState
  { _frameClock :: !Clock
  , _screen     :: !GameScreen
  , _drawables  :: ![Drawable]
  , _setup      :: !Bool
  }

type GameM a = StateT GameState IO a

makeLenses ''GameState

changeScreen :: GameScreen -> GameM ()
changeScreen gs = modify $
  (set drawables []) . (set screen gs) . (set setup True)

setupDone :: GameM ()
setupDone = modify $ set setup False

debugP :: String -> GameM ()
debugP = liftIO . putStrLn . ((++) "[DEBUG] ")

errorP :: String -> IO ()
errorP msg = do
  putStrLn $ "[ERROR] " ++ msg
  exitFailure

