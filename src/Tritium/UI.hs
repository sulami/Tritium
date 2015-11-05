module Tritium.UI (
  addText, centerText, centerTextX, newText, positionText
) where

import           Control.Monad.State (get, liftIO, modify, put)

import           Control.Lens ((^.), (|>), both, over, set, view)
import           SFML.SFException (SFException (..))
import           SFML.Graphics.Font (fontFromFile)
import           SFML.Graphics.Rect (FloatRect (..))
import qualified SFML.Graphics.Text as T
import           SFML.Graphics.SFTransformable (getPosition, setPosition)
import           SFML.Graphics.Types (Font (..), Text (..))
import qualified SFML.System.Vector2 as V2

import           Paths_tritium (getDataFileName)
import           Tritium.Import

-- | Load a font from the resource directory.
loadFont :: String -> IO (Maybe Font)
loadFont name = do
  path <- getDataFileName ("res/" ++ name ++ ".ttf")
  font <- fontFromFile path
  case font of
    Left (SFException msg) -> do errorP msg
                                 return Nothing
    Right fnt              -> return $ Just fnt

-- | Create a new text object using the specified font, size and content.
newText :: String -> Int -> String -> GameM (Maybe Text)
newText name size str = do
  Just font <- liftIO $ loadFont name
  text <- liftIO $ T.createText
  case text of
    Left (SFException msg) -> do liftIO $ errorP msg
                                 return Nothing
    Right txt              -> do
      liftIO $ do T.setTextString txt str
                  T.setTextFont txt font
                  T.setTextCharacterSize txt size
      return $ Just txt

-- | Add a text to the drawable list.
addText :: Text -> GameM ()
addText t = modify $ \s -> set drawables (s^.drawables |> (DText t)) s

-- | Position a text at the specified position, anchored at the center.
positionText :: Text -> (Float, Float) -> GameM ()
positionText txt (x,y) = do
  FloatRect _ _ ow oh <- liftIO $ T.getTextGlobalBounds txt
  liftIO . setPosition txt $ V2.Vec2f (x - ow/2) (y - oh/2)

-- | Center a text in the x-dimension, preserving the y-coordinate.
centerTextX :: Text -> GameM ()
centerTextX txt = do
  (windowWidth, _) <- windowSize
  let windowCenter = fromIntegral $ windowWidth `div` 2
  V2.Vec2f _ objY <- liftIO $ getPosition txt
  positionText txt (windowCenter, objY)

-- | Center a text on the screen.
centerText :: Text -> GameM ()
centerText txt = do
  windowDimensions <- windowSize
  let coords = over both ((/ 2) . fromIntegral) windowDimensions
  positionText txt coords

