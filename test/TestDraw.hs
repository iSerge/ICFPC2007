module TestDraw (testDraw) where

import Color
import Control.DeepSeq
import Data.Array.IO
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (rectangle)
import System.IO

testDraw :: IO ()
testDraw = do
  putStrLn "Testing color:"
  putStr "Empty bucket: "
  print $ currentPixel []
  putStr "[t, o, o]: "
  print $ currentPixel [transparent, opaque, opaque]
  putStr "[b, y, c]: "
  print $ currentPixel [black, yellow, cyan]
  putStr "[y, t, o]: "
  print $ currentPixel [yellow, transparent, opaque]

test = do
  arr <- newArray ((0, 0), (3, 3)) (0, 0, 0, 0) :: IO (IOArray (Int, Int) Pixel)
  a <- readArray arr (0, 0)
  writeArray arr (0, 0) (64, 0, 0, 0)
  b <- readArray arr (0, 0)
  print (a, b)

test2 = do
  inFile <- openFile "foo" ReadMode
  contents <- hGetContents inFile
  contents `deepseq` hClose inFile
  putStr contents

test3 :: IO ()
test3 = do
  initGUI
  window <- windowNew
  set
    window
    [ windowTitle := "Save as...",
      windowDefaultWidth := 300,
      windowDefaultHeight := 200
    ]

  let pnw = 300
      pnh = 200
  withImageSurface
    FormatARGB32
    pnw
    pnh
    ( \srf -> do
        renderWith srf (myDraw (fromIntegral pnw) (fromIntegral pnh))
        surfaceWriteToPNG srf "myDraw.png"
    )

  let pdw = 720
      pdh = 720
  withPDFSurface
    "myDraw.pdf"
    pdw
    pdh
    ( \s -> renderWith s $ do
        myDraw pdw pdh
        showPage
    )

  let psw = 360
      psh = 540
  withPSSurface
    "myDraw.ps"
    psw
    psh
    (flip renderWith (myDraw psw psh >> showPage))

  let pgw = 180
      pgh = 360
  withSVGSurface
    "myDraw.svg"
    pgw
    pgh
    (flip renderWith $ myDraw pgw pgh >> showPage)

  putStrLn "Press any key to quit..."
  -- window `on` key
  -- onKeyPress
  --   window
  --   ( \x -> do
  --       widgetDestroy window
  --       return True
  --   )

  widgetShowAll window
  window `on` objectDestroy $ mainQuit
  mainGUI

myDraw :: Double -> Double -> Render ()
myDraw w h = do
  setSourceRGB 1 1 1
  paint

  setSourceRGB 0 0 0
  moveTo 0 0
  lineTo w h
  moveTo w 0
  lineTo 0 h
  setLineWidth (0.1 * (h + w))
  stroke

  rectangle 0 0 (0.5 * w) (0.5 * h)
  setSourceRGBA 1 0 0 0.8
  fill

  rectangle 0 (0.5 * h) (0.5 * w) (0.5 * h)
  setSourceRGBA 0 1 0 0.6
  fill

  rectangle (0.5 * w) 0 (0.5 * w) (0.5 * h)
  setSourceRGBA 0 0 1 0.4
  fill
