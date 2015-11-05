{-# LANGUAGE TemplateHaskell #-}

module Tritium.Import where

import           Control.Monad.State (StateT, get, liftIO, modify)
import           System.Exit (exitFailure)

import           Control.Lens (makeLenses, set, view)
import qualified SFML.Graphics.RenderWindow as RW
import qualified SFML.Graphics.Types as GT
import           SFML.System.Clock (Clock)
import           SFML.System.Time (Time)
import qualified SFML.System.Vector2 as V2
import           SFML.Window.Event (SFEvent (..))

-- | The type each screen function has. It gets the time passed since rendering
-- the last frame for FPS-independent speeds and possibly events to handle.
type GameScreen = Time -> Maybe SFEvent -> GameM ()

-- | Different drawable objects that can be added to be rendered every frame.
data Drawable = DShape GT.Shape
              | DSprite GT.Sprite
              | DText GT.Text

-- | Draw a drawable.
draw :: GT.RenderWindow -> Drawable -> IO ()
draw win (DShape s)  = RW.drawShape win s Nothing
draw win (DSprite s) = RW.drawSprite win s Nothing
draw win (DText t)   = RW.drawText win t Nothing

-- | The more or less global game state that holds everything that could be
-- needed at runtime. Uses lenses for easy access.
data GameState = GameState
  { _frameClock :: !Clock
  , _screen     :: !GameScreen
  , _drawables  :: ![Drawable]
  , _setup      :: !Bool
  , _window     :: !GT.RenderWindow
  }

-- | The game monad.
type GameM a = StateT GameState IO a

makeLenses ''GameState

-- | Function to be called when switching the screen, to clear the drawable
-- objects, set the flag to setup the new list and update the screen.
changeScreen :: GameScreen -> GameM ()
changeScreen gs = modify $
  (set drawables []) . (set screen gs) . (set setup True)

-- | Function to be called after done setting up the new screen.
setupDone :: GameM ()
setupDone = modify $ set setup False

-- | Get the current window size.
windowSize :: GameM (Word, Word)
windowSize = do
  win <- view window <$> get
  (V2.Vec2u w h) <- liftIO $ RW.getWindowSize win
  return (w,h)

-- | Debug prints.
debugP :: String -> GameM ()
debugP = liftIO . putStrLn . ((++) "[DEBUG] ")

-- | Error prints. Kill the main process.
errorP :: String -> IO ()
errorP msg = do
  putStrLn $ "[ERROR] " ++ msg
  exitFailure

