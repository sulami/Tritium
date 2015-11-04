module Tritium.UI (
  addText, centerText, newText
) where

import           Control.Monad.State (get, liftIO, modify, put)
import           Data.Maybe (fromJust)

import           Control.Lens ((^.), (|>), both, over, set, view)
import           SFML.Graphics.Font (FontException (..), fontFromFile)
import           SFML.Graphics.Rect (FloatRect (..))
import qualified SFML.Graphics.Text as T
import           SFML.Graphics.Transformable (Transformable, setPosition)
import           SFML.Graphics.Types (Font (..), Text (..))
import qualified SFML.System.Vector2 as V2

import           Paths_tritium (getDataFileName)
import           Tritium.Import

loadFont :: String -> IO (Maybe Font)
loadFont name = do
  path <- getDataFileName ("res/" ++ name ++ ".ttf")
  font <- fontFromFile path
  case font of
    Left (FontException msg) -> do errorP msg
                                   return Nothing
    Right fnt                -> return $ Just fnt

newText :: String -> Int -> String -> GameM (Maybe Text)
newText name size str = do
  font <- liftIO $ fromJust <$> loadFont name
  text <- liftIO $ T.createText
  case text of
    Left (T.TextException msg) -> do liftIO $ errorP msg
                                     return Nothing
    Right txt                  -> do
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

