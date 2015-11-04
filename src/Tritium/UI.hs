module Tritium.UI (
  newText
) where

import           Control.Monad.State (get, liftIO, modify, put)
import           Data.Maybe (fromJust)

import           Control.Lens ((^.), (<|), set)
import           SFML.Graphics.Font (FontException (..), fontFromFile)
import qualified SFML.Graphics.Text as T
import           SFML.Graphics.Types (Font (..), Text (..))

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

newText :: String -> Int -> String -> GameM ()
newText name size str = do
  font <- liftIO $ fromJust <$> loadFont name
  text <- liftIO $ T.createText
  case text of
    Left (T.TextException msg) -> liftIO $ errorP msg
    Right txt                  -> do
      liftIO $ do T.setTextStringU txt str
                  T.setTextFont txt font
                  T.setTextCharacterSize txt size
      modify $ \s -> set drawables ((DText txt) <| s^.drawables) s

