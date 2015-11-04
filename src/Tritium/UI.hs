module Tritium.UI (
) where

import           Control.Monad.State (liftIO)
import           Data.Maybe (fromJust)

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
newText name size str = liftIO $ do
  font <- fromJust <$> loadFont name
  text <- T.createText
  case text of
    Left (T.TextException msg) -> errorP msg
    Right txt                  -> do
      T.setTextStringU txt str
      T.setTextFont txt font
      T.setTextCharacterSize txt size

