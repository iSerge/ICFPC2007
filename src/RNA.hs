module RNA
  ( RNA,
    RNAop
      ( AddColor,
        EmptyBucket,
        Move,
        TurnCW,
        TurnCCW,
        Mark,
        Line,
        Fill,
        AddBitmap,
        Compose,
        Clip,
        OtherRNA
      ),
    emptyRNA,
    parseRNA,
    parseRnaOp,
  )
where

import Color
import Data.Sequence as S

data RNAop
  = AddColor Color
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

type RNA = S.Seq RNAop

emptyRNA :: RNA
emptyRNA = S.empty

parseRnaOp :: String -> RNAop
parseRnaOp str | 7 <- Prelude.length str = case str of
  "PIPIIIC" -> AddColor black
  "PIPIIIP" -> AddColor red
  "PIPIICC" -> AddColor green
  "PIPIICF" -> AddColor yellow
  "PIPIICP" -> AddColor blue
  "PIPIIFC" -> AddColor magenta
  "PIPIIFF" -> AddColor cyan
  "PIPIIPC" -> AddColor white
  "PIPIIPF" -> AddColor transparent
  "PIPIIPP" -> AddColor opaque
  "PIIPICP" -> EmptyBucket
  "PIIIIIP" -> Move
  "PCCCCCP" -> TurnCCW
  "PFFFFFP" -> TurnCW
  "PCCIFFP" -> Mark
  "PFFICCP" -> Line
  "PIIPIIP" -> Fill
  "PCCPFFP" -> AddBitmap
  "PFFPCCP" -> Compose
  "PFFICCF" -> Clip
  _ -> OtherRNA
parseRnaOp _ = OtherRNA

parseRNA :: String -> RNA
parseRNA [] = emptyRNA
parseRNA str = parseRnaOp strChunk S.<| parseRNA restStr
  where
    strChunk = Prelude.take 7 str
    restStr = Prelude.drop 7 str
