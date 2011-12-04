-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Graphics.UI.Gtk hiding (get, Bitmap)
import Graphics.Rendering.Cairo hiding (clip)
import Control.Monad
import Control.Monad.State
import Control.DeepSeq
import Data.Array.MArray
import Data.Word
import GHC.Float
import System
import System.IO
import Color
import RNA
import Direction

data ProcState = ProcState {
                        bucket :: !Bucket,
                        currPix :: !Pixel,
                        pos :: !(Int, Int),
                        mark :: !(Int, Int),
                        dir :: !Direction,
                        bitmaps :: ![Bitmap],
                        iteration :: !Integer
                    } -- deriving (Show)

initState = ProcState [] (0,0,0,255) (0,0) (0,0) E [] 0

readRNA :: String -> IO RNA
readRNA file = do
  str <- readFile file
  return $ parseRNA str

turnCW :: StateT ProcState IO ()
turnCW = do
  s <- get
  case (dir s) of
    N -> put s{dir = E}
    E -> put s{dir = S}
    S -> put s{dir = W}
    W -> put s{dir = N}

turnCCW :: StateT ProcState IO ()
turnCCW = do
  s <- get
  case (dir s) of
    N -> put s{dir = W}
    E -> put s{dir = N}
    S -> put s{dir = E}
    W -> put s{dir = S}

move :: StateT ProcState IO ()
move = do
  s <- get
  let p = pos s in
    case (dir s) of
      N -> if snd p == 0 then put s{pos = (fst p, 599)} else put s{pos = (fst p, snd p - 1)}
      E -> if fst p == 599 then put s{pos = (0, snd p)} else put s{pos = (fst p + 1, snd p)}
      S -> if snd p == 599 then put s{pos = (fst p, 0)} else put s{pos = (fst p, snd p + 1)}
      W -> if fst p == 0 then put s{pos = (599, snd p)} else put s{pos = (fst p - 1, snd p)}

setMark :: StateT ProcState IO ()
setMark = do
  s <- get
  put s{mark = pos s}

addColor :: Color.Color -> StateT ProcState IO ()
addColor c = do
  s <- get
  let b = bucket s
    in put s{bucket = c : b, currPix = currentPixel (c:b)}

clearBucket :: StateT ProcState IO ()
clearBucket = do
  s <- get
  put s{bucket = [], currPix = (0,0,0,255)}

currentPixelS :: StateT ProcState IO Pixel
currentPixelS = do
  s <- get
  return $ currPix s

addBitmap :: StateT ProcState IO ()
addBitmap = do
  s <- get
  when (length (bitmaps s) < 10) $
    do
      bitmap <- liftIO transparentBitmap
      put s{bitmaps = bitmap : bitmaps s}

{-
getP bitmap pos = bitmap!pos

setP :: Bitmap -> (Int, Int) -> Pixel -> Bitmap
setP bitmap pos pixel =
  let f = \p -> if p == pos then pixel else bitmap!pos
    in array bitmapBounds [(i, f i) | i <- range bitmapBounds]
-}

getPixel :: Position -> StateT ProcState IO Pixel
getPixel pos = do
  s <- get
  let b = head $ bitmaps s
  pixel <- liftIO $ readPixel b pos
  pixel `deepseq` return pixel

setPixel pos = do
  --liftIO $ putStrLn ("setPixel " ++ show pos)
  s <- get
  p <- currentPixelS
  let bb = bitmaps s
      b = head bb
  r <- liftIO $ writePixel b pos p
  r `deepseq` return ()

draw = do
  s <- get
  let
       (px, py) = pos s
       (mx, my) = mark s
       deltax = mx - px
       deltay = my - py
       d = max (abs deltax) (abs deltay)
       c = if (deltax * deltay) <= 0 then 1 else 2
       x = (px * d) + ((d - c) `div` 2)
       y = (py * d) + ((d - c) `div` 2)
       lineIter = \x y i -> unless (i == 0) $
                 do
                   setPixel (x `div` d, y `div` d)
                   lineIter (x + deltax) (y + deltay) (i - 1)
  --liftIO $ putStrLn ("pos " ++ show (px, py))
  --liftIO $ putStrLn ("mark " ++ show (mx, my))
  lineIter x y d
  setPixel (mx, my)

tryFill = do
  s <- get
  new <- currentPixelS
  let p = pos s
      --bitmap = head $ bitmaps s
  old <- getPixel p
  when (new /= old) $ fillIter old [p]

fillIter :: Pixel -> [Position] -> StateT ProcState IO ()
fillIter initial [] = return ()
fillIter initial (p:ps) = do
  pix <- getPixel p
  psN <- if pix == initial
    then do
      let
        (x, y) = p
        ps'    = if y < 599 then (x, y+1):ps    else ps
        ps''   = if y > 0   then (x, y-1):ps'   else ps'
        ps'''  = if x < 599 then (x+1, y):ps''  else ps''
        ps'''' = if x > 0   then (x-1, y):ps''' else ps'''
      setPixel p
      return ps'''
    else return ps
  fillIter initial psN
{-
fill (x, y) initial = do
  p <- getPixel (x, y)
  if (p == initial)
    then do
      setPixel (x, y)
      if x > 0 then fill (x - 1, y) initial else do return ()
      if x < 599 then fill (x + 1, y) initial else do return ()
      if y > 0 then fill (x, y - 1) initial else do return ()
      if y < 599 then fill (x, y + 1) initial else do return ()
    else return ()
-}

composeFunc :: Pixel -> Pixel -> Pixel
composeFunc pix0 pix1 = (f r0 r1 , f g0 g1, f b0 b1, f a0 a1) where
  (r0, g0, b0, a0) = pix0
  (r1, g1, b1, a1) = pix1
  f = \c0 c1 -> c0 + ((c1 * (255 - a0)) `div` 255)

clipFunc :: Pixel -> Pixel -> Pixel
clipFunc pix0 pix1 = (f r1 , f g1, f b1, f a1) where
  (r0, g0, b0, a0) = pix0
  (r1, g1, b1, a1) = pix1
  f = \c1 -> (c1 * a0) `div` 255

pixelTransform :: (Pixel -> Pixel -> Pixel) -> Bitmap -> Bitmap -> Position -> StateT ProcState IO ()
pixelTransform f b0 b1 pos = do
  p0 <- liftIO $ readPixel b0 pos
  p1 <- liftIO $ readPixel b1 pos
  let newPix = f p0 p1
  r  <- newPix `deepseq` liftIO $ writePixel b1 pos newPix
  r `deepseq` return ()

bitmapProcess f = do
  s <- get
  when (length (bitmaps s) > 1) $
    let bb = bitmaps s
        b0 = head bb
        b1 = head $ tail bb
        {-
        proc :: Position -> StateT ProcState IO ()
        proc = \pos -> do
               p0 <- liftIO $ readPixel b0 pos
               p1 <- liftIO $ readPixel b1 pos
               let newPix = f p0 p1
               r  <- newPix `deepseq` liftIO $ writePixel b1 pos newPix
               r `deepseq` return ()
        -}
        proc_i [] = return ()
        proc_i (pos:rng) = do
               p0 <- liftIO $ readPixel b0 pos
               p1 <- liftIO $ readPixel b1 pos
               let newPix = f p0 p1
               r  <- newPix `deepseq` liftIO $ writePixel b1 pos newPix
               r `deepseq` proc_i rng
      in do
        mapM_ (pixelTransform f b0 b1) $ range bitmapBounds
        --proc_i $ range bitmapBounds
        put s{bitmaps = tail bb}

compose = bitmapProcess composeFunc

clip = bitmapProcess clipFunc

printIteration :: StateT ProcState IO ()
printIteration = do
  s <- get
  liftIO $ putStrLn ("Iteration: " ++ show (iteration s))

incIteration = do
  s <- get
  put s{iteration = 1 + iteration s}

savePixmap b file = do
  p <- getPixbuf b
  withImageSurface
       FormatARGB32 600 600 (\srf -> do renderWith srf (drawPixmap p)
                                        surfaceWriteToPNG srf $ file++".png")

getPixbuf :: Bitmap -> IO Pixbuf
getPixbuf b = do
  pbuf <- pixbufNew ColorspaceRgb False 8 600 600
  rowstride <- pixbufGetRowstride pbuf
  nChannels <- pixbufGetNChannels pbuf
  pixels    <- (pixbufGetPixels pbuf :: IO (PixbufData Int Word8))
  let proc_i [] = return ()
      proc_i ((x,y):rng) = do
        (r, g, b, a) <- liftIO $ readPixel b (x,y)
        let p = y*rowstride + x*nChannels
        res   <- r `deepseq` writeArray pixels p $ fromInteger $ toInteger r
        res'  <- res  `deepseq` g `deepseq` writeArray pixels (p+1) $ fromInteger $ toInteger g
        res'' <- res' `deepseq` b `deepseq` writeArray pixels (p+2) $ fromInteger $ toInteger b
        res'' `deepseq` proc_i rng
  proc_i $ range bitmapBounds
  return pbuf

drawPixmap p = do
  setSourcePixbuf p 0 0
  paint

startRNAProc [] = return ()
startRNAProc (file:files) = do
  liftIO $ putStrLn $ "Processing file: " ++ file
  rna <- liftIO $ readRNA file
  addBitmap
  processRNA rna
  --pixmap <- getPixbuf
  --liftIO $ savePixmap pixmap file
  s<- get
  let b = head $ bitmaps s
  liftIO $ savePixmap b file
  startRNAProc files

processRNA :: RNA -> StateT ProcState IO ()
processRNA [] = return ()
processRNA (op:rna) = do
--  printIteration
  case op of
    AddColor color -> do
--            liftIO $ putStrLn ("addColor: " ++ show color)
--            liftIO $ hFlush stdout
            addColor color
    EmptyBucket -> do
--            liftIO $ putStrLn "clearBucket"
--            liftIO $ hFlush stdout
            clearBucket
    Move -> do
--            liftIO $ putStrLn "move"
--            liftIO $ hFlush stdout
            move
    TurnCW -> do
--            liftIO $ putStrLn "turnCW"
--            liftIO $ hFlush stdout
            turnCW
    TurnCCW -> do
--            liftIO $ putStrLn "turnCCW"
--            liftIO $ hFlush stdout
            turnCCW
    Mark -> do
--            liftIO $ putStrLn "setMark"
--            liftIO $ hFlush stdout
            setMark
    Line -> do
--            liftIO $ putStrLn "line"
--            liftIO $ hFlush stdout
            draw
    RNA.Fill -> do
--            liftIO $ putStrLn "fill"
--            liftIO $ hFlush stdout
            tryFill
    AddBitmap -> do
--            liftIO $ putStrLn "addBitmap"
--            liftIO $ hFlush stdout
            addBitmap
    Compose -> do
--            liftIO $ putStrLn "compose"
--            liftIO $ hFlush stdout
            compose
    Clip -> do
--            liftIO $ putStrLn "clip"
--            liftIO $ hFlush stdout
            clip
    OtherRNA -> return ()
  incIteration
  processRNA rna

main = do
  args <- getArgs
  initGUI
  --rna <- readRNA "../../test/selfcheck.dna"
  runStateT (startRNAProc args) initState

{-
main = do
  rna <- readRNA "../../test/selfcheck.dna"
  initGUI
  window <- windowNew
  vbox    <- vBoxNew True 10
  startBtn <- buttonNewWithLabel "Start processing"
  set window [windowDefaultWidth := 200,
              windowDefaultHeight := 200,
              containerChild := vbox,
              windowTitle := "RNA processor"]
  boxPackStart vbox startBtn PackGrow 0
  onClicked startBtn (processRNA rna)
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
-}

--while (gtk_events_pending ())
--  gtk_main_iteration ();

--  gtk_main_iteration_do (FALSE);
