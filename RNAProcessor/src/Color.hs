module Color (
Color, Bucket, Pixel, Bitmap,
black, red, green, yellow, blue, magenta, cyan, white, transparent, opaque,
currentPixel, transparentBitmap, bitmapBounds, readPixel, writePixel
) where

import Data.Array.IO
import Direction

data Color = RGB Int Int Int | Transparency Int
                deriving (Show)

type Bucket = [Color]

type Pixel = (Int, Int, Int, Int)

type Bitmap = IOArray Position Pixel

black       = RGB 0 0 0
red         = RGB 255 0 0
green       = RGB 0 255 0
yellow      = RGB 255 255 0
blue        = RGB 0 0 255
magenta     = RGB 255 0 255
cyan        = RGB 0 255 255
white       = RGB 255 255 255
transparent = Transparency 0
opaque      = Transparency 255

currentPixel:: Bucket -> Pixel
currentPixel [] = (0, 0, 0, 255)
currentPixel bucket = currentPixelIter bucket (black, 0, 0, 0)

currentPixelIter :: Bucket -> (Color, Int, Int, Int) -> Pixel
currentPixelIter [] (RGB r g b, cn, a, an) = ((r `div` cn')*(a' `div` an') `div` 255,
                                            (g `div` cn')*(a' `div` an') `div` 255,
                                            (b `div` cn')*(a' `div` an') `div` 255,
                                            a' `div` an')
     where cn' = if cn == 0 then 1 else cn
           an' = if an == 0 then 1 else an
           a'  = if an == 0 then 255 else a

currentPixelIter (color:bucket) (RGB r g b, cn, a, an) = case color of
  RGB r' g' b' -> currentPixelIter bucket (RGB (r+r') (g+g') (b+b'), cn + 1, a, an)
  Transparency a'  -> currentPixelIter bucket (RGB r g b, cn, a + a', an + 1)

bitmapBounds :: (Position, Position)
bitmapBounds = ((0,0), (599,599))

readPixel :: Bitmap -> Position -> IO Pixel
readPixel = readArray

writePixel :: Bitmap -> Position -> Pixel -> IO ()
writePixel = writeArray
--writePixel = unsafeWrite

transparentBitmap :: IO Bitmap
transparentBitmap = newArray bitmapBounds (0,0,0,0) :: IO (IOArray Position Pixel)
-- array ((0,0), (599,599)) [(i,(0,0,0,0)) | i <- range ((0,0), (599,599))]
