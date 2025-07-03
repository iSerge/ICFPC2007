module Main (main) where

import Graphics.UI.Gtk
import GUI

main :: IO ()
main = do
  _ <- initGUI
  u <- setupGUI
  widgetShowAll $ mainWindow u
  mainGUI
