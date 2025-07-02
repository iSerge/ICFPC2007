module Color
  ( Color,
    Bucket,
    Pixel,
    Bitmap,
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    transparent,
    opaque,
    currentPixel,
    transparentBitmap,
    bitmapBounds,
    readPixel,
    writePixel,
  )
where

import Data.Array.IO
import Direction

data Color = RGB !Int !Int !Int | Transparency !Int
  deriving (Show)

type Bucket = [Color]

type Pixel = (Int, Int, Int, Int)

type Bitmap = IOArray Position Pixel

black :: Color
black = RGB 0 0 0

red :: Color
red = RGB 255 0 0

green :: Color
green = RGB 0 255 0

yellow :: Color
yellow = RGB 255 255 0

blue :: Color
blue = RGB 0 0 255

magenta :: Color
magenta = RGB 255 0 255

cyan :: Color
cyan = RGB 0 255 255

white :: Color
white = RGB 255 255 255

transparent :: Color
transparent = Transparency 0

opaque :: Color
opaque = Transparency 255

currentPixel :: Bucket -> Pixel
currentPixel [] = (0, 0, 0, 255)
currentPixel bucket = currentPixelIter (black, 0, 0, 0) bucket

currentPixelIter :: (Color, Int, Int, Int) -> Bucket -> Pixel
currentPixelIter (RGB r g b, cn, a, an) [] =
  ( (r `div` cn') * (a' `div` an') `div` 255,
    (g `div` cn') * (a' `div` an') `div` 255,
    (b `div` cn') * (a' `div` an') `div` 255,
    a' `div` an'
  )
  where
    cn' = if cn == 0 then 1 else cn
    an' = if an == 0 then 1 else an
    a' = if an == 0 then 255 else a
currentPixelIter (RGB r g b, cn, a, an) (RGB r' g' b' : bucket) =
  currentPixelIter (RGB (r + r') (g + g') (b + b'), cn + 1, a, an) bucket
currentPixelIter (RGB r g b, cn, a, an) (Transparency a' : bucket) =
  currentPixelIter (RGB r g b, cn, a + a', an + 1) bucket

bitmapBounds :: (Position, Position)
bitmapBounds = ((0, 0), (599, 599))

readPixel :: Bitmap -> Position -> IO Pixel
readPixel = readArray

writePixel :: Bitmap -> Position -> Pixel -> IO ()
writePixel = writeArray

-- writePixel = unsafeWrite

transparentBitmap :: IO Bitmap
transparentBitmap = newArray bitmapBounds (0, 0, 0, 0) :: IO (IOArray Position Pixel)

-- array ((0,0), (599,599)) [(i,(0,0,0,0)) | i <- range ((0,0), (599,599))]
