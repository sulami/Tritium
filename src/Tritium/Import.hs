module Tritium.Import where

import           Graphics.Rendering.OpenGL (GLdouble)
import           Graphics.UI.Fungen

data GameState = MainMenu

type TritiumAction a = IOGame () () GameState () a

