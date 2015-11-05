module Tritium.UI (
  addText, centerText, newText
) where

import           Control.Monad.State (get, liftIO, modify, put)

import           Control.Lens ((^.), (|>), both, over, set, view)
import           SFML.SFException (SFException (..))
import           SFML.Graphics.Font (fontFromFile)
import           SFML.Graphics.Rect (FloatRect (..))
import qualified SFML.Graphics.Text as T
import           SFML.Graphics.SFTransformable (setPosition)
import           SFML.Graphics.Types (Font (..), Text (..))
import qualified SFML.System.Vector2 as V2

import           Paths_tritium (getDataFileName)
import           Tritium.Import

loadFont :: String -> IO (Maybe Font)
loadFont name = do
  path <- getDataFileName ("res/" ++ name ++ ".ttf")
  font <- fontFromFile path
  case font of
    Left (SFException msg) -> do errorP msg
                                 return Nothing
    Right fnt              -> return $ Just fnt

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

addText :: Text -> GameM ()
addText t = modify $ \s -> set drawables (s^.drawables |> (DText t)) s

centerText :: Text -> GameM ()
centerText txt = do
  windowDimensions <- windowSize
  FloatRect _ _ ow oh <- liftIO $ T.getTextGlobalBounds txt
  let (x,y) = over both ((/ 2) . fromIntegral) windowDimensions
  liftIO . setPosition txt $ V2.Vec2f (x - ow/2) (y - oh)

