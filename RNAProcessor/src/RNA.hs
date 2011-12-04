module RNA (
RNA,
RNAop (AddColor, EmptyBucket, Move, TurnCW, TurnCCW, Mark,
       Line, Fill, AddBitmap, Compose, Clip, OtherRNA),
parseRNA
) where

import Color

data RNAop = AddColor Color
           | EmptyBucket
           | Move
           | TurnCW
           | TurnCCW
           | Mark
           | Line
           | Fill
           | AddBitmap
           | Compose
           | Clip
           | OtherRNA
                deriving (Show)

type RNA = [RNAop]

parseRNA:: String -> RNA
parseRNA [] = []
parseRNA str = case strChunk of
    "PIPIIIC" -> AddColor black : parseRNA restStr
    "PIPIIIP" -> AddColor red : parseRNA restStr
    "PIPIICC" -> AddColor green : parseRNA restStr
    "PIPIICF" -> AddColor yellow : parseRNA restStr
    "PIPIICP" -> AddColor blue : parseRNA restStr
    "PIPIIFC" -> AddColor magenta : parseRNA restStr
    "PIPIIFF" -> AddColor cyan : parseRNA restStr
    "PIPIIPC" -> AddColor white : parseRNA restStr
    "PIPIIPF" -> AddColor transparent : parseRNA restStr
    "PIPIIPP" -> AddColor opaque : parseRNA restStr
    "PIIPICP" -> EmptyBucket : parseRNA restStr
    "PIIIIIP" -> Move : parseRNA restStr
    "PCCCCCP" -> TurnCCW : parseRNA restStr
    "PFFFFFP" -> TurnCW : parseRNA restStr
    "PCCIFFP" -> Mark : parseRNA restStr
    "PFFICCP" -> Line : parseRNA restStr
    "PIIPIIP" -> Fill : parseRNA restStr
    "PCCPFFP" -> AddBitmap : parseRNA restStr
    "PFFPCCP" -> Compose : parseRNA restStr
    "PFFICCF" -> Clip : parseRNA restStr
    _         -> OtherRNA : parseRNA restStr
  where (strChunk, restStr) = chomp str 7


chomp str n = chompIter str n []

chompIter :: String -> Int -> String -> (String, String)
chompIter []      n r = (reverse r, "")
chompIter str     0 r = (reverse r, str)
chompIter (s:str) n r = chompIter str (n - 1) (s:r)

