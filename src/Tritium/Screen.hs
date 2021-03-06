module Tritium.Screen (
  mainMenu
) where

import           Control.Monad (when)
import           Control.Monad.State (get, modify)

import           Control.Lens ((^.))
import           SFML.Window.Event (SFEvent (..))

import           Tritium.Import
import           Tritium.UI

-- | The main menu.
mainMenu :: GameScreen
mainMenu ft ev = do
  state <- get
  when (state^.setup) $ do
    Just title <- newText "MonkirtaPursuit" 90 "Tritium"
    centerTextX title
    addText title
    setupDone
  case ev of
    Just (SFEvtMouseButtonPressed mbtn mx my) ->
      debugP $ "Click " ++ show mbtn ++ "@" ++ show mx ++ "," ++ show my
    _ -> return ()

