module Main where

--import Graphics.UI.Gtk
import DNAProcessor
import Control.Monad.State
import ProcessorState

processDNA :: IO ()
processDNA = do runStateT (exec "IIPIFFCPICICIICPIICIPPPICIIC" "../../endo.dna") initVars -- Self check
--processDNA = do runStateT (exec "IIPIFFCPICFPPICIICCIICIPPPFIIC" "../../endo.dna") initVars -- first prefix
--processDNA = do runStateT (exec "" "../../endo.dna") initVars -- Sad Endo
--processDNA = do runStateT (exec "IIPIFFCPICICIICPIICIPPPICIIC" "../../endo.dna") initVars -- ??
--processDNA = do runStateT (exec "IIPIFFCPICFPPICIICCCIICIPPPCFIIC" "../../endo.dna") initVars -- Manual index
                return ()

--hello :: (ButtonClass o) => o -> IO ()
--hello b = set b [buttonLabel := "Hello World"]

main :: IO ()
main = processDNA
{-
main = do
  unsafeInitGUIForThreadedRTS --initGUI
  window <- windowNew
  hbox    <- hBoxNew True 10
  processBtn <- buttonNewWithLabel "Start processing"
  button2 <- buttonNewWithLabel "Button 2"
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerChild := hbox, containerBorderWidth := 10]
  boxPackStart hbox processBtn PackGrow 0
  boxPackStart hbox button2 PackGrow 0
  onClicked processBtn processDNA
  onClicked button2 $ hello button2
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
-}
