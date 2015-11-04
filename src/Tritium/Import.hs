module Tritium.Import where

import           Control.Monad.State (StateT, liftIO)
import           System.Exit (exitFailure)

import qualified SFML.Graphics.RenderWindow as RW
import qualified SFML.Graphics.Types as GT
import           SFML.System.Time (Time)
import           SFML.System.Clock (Clock)
import           SFML.Window.Event (SFEvent (..))

type GameScreen = Time -> Maybe SFEvent -> GameM ()

data Drawable = Shape GT.Shape
              | Sprite GT.Sprite
              | Text GT.Text

draw :: GT.RenderWindow -> Drawable -> IO ()
draw win (Shape s)  = RW.drawShape win s Nothing
draw win (Sprite s) = RW.drawSprite win s Nothing
draw win (Text t)   = RW.drawText win t Nothing

data GameState = GameState
  { frameClock :: !Clock
  , screen     :: !GameScreen
  , drawables  :: ![Drawable]
  }

type GameM a = StateT GameState IO a

debugP :: String -> GameM ()
debugP = liftIO . putStrLn . ((++) "[DEBUG] ")

errorP :: String -> IO ()
errorP msg = do
  putStrLn $ "[ERROR] " ++ msg
  exitFailure

