module Tritium.Menu (
  mainMenu,
  menuImages, menuButtons,
  createButton
) where

import           Graphics.Rendering.OpenGL (GLdouble)
import           Graphics.UI.Fungen

import           Tritium.Import

type MenuButton = (String, Int, (GLdouble, GLdouble))

menuImages :: FilePictureList
menuImages = [ ("btnnewgame", Nothing)
             , ("btnoptions", Nothing)
             , ("btnquit",    Nothing) ]

menuButtons :: [MenuButton]
menuButtons = [ ("new game",  0,  (400,260))
              , ("options",   1,  (400,180))
              , ("quit",      2,  (400,100)) ]

createButton :: MenuButton -> Button
createButton (name, texId, (x,y)) =
  object name (Tex (200,50) texId) False (x,y) (0,0) ()

mainMenu :: TritiumAction ()
mainMenu = do
  quitBtn <- findObject "quit" "buttons"
  drawObject quitBtn

