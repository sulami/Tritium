module Tritium.Import where

import           Graphics.Rendering.OpenGL (GLdouble)
import           Graphics.UI.Fungen

import           Paths_tritium (getDataFileName)

data GameState = MainMenu

type Button = GameObject ()

type TritiumAction a = IOGame () () GameState () a

loadImage :: (FilePath, InvList) -> IO (FilePath, InvList)
loadImage (p,i) = do
  p' <- getDataFileName ("res/" ++ p ++ ".bmp")
  return (p', i)

