module ProcessorState where

import DNA
import qualified Data.Sequence as S
import Control.Monad
import Control.Monad.State
import qualified Data.Foldable as F

data PItem = Pi Base | LParen | RParen | Skip Integer | Search DNA
  deriving (Eq, Show)

type RNA = S.Seq DNA

rna2String :: RNA -> String
rna2String = F.foldl (\a b -> a ++ dna2String b) ""

type Pattern = S.Seq PItem

pat2String :: Pattern -> String
pat2String = F.foldl' (flip $ flip (++) . pItem2String) ""

pItem2String (Pi b) = [base2Char b]
pItem2String LParen = "("
pItem2String RParen = ")"
pItem2String (Skip n) = '!' : show n
pItem2String (Search d) = "?[" ++ dna2String d ++ "]"

data TItem = Ti Base | Protected Integer Integer | LengthOf Integer
  deriving (Eq, Show)

type Template = S.Seq TItem

tpl2String :: Template -> String
tpl2String= F.foldl' (flip $ flip (++) . tItem2String) ""

tItem2String (Ti b)          = [base2Char b]
tItem2String (Protected n l) = "[" ++ show n ++ "^" ++ show l ++ "]"
tItem2String (LengthOf n)    = "|" ++ show n ++ "|"
