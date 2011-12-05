module ProcessorState where

import DNA
import DNASeq
--import DNAByteStr
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

data ProcVars = ProcVars {
    dna :: DNA,
    cost :: !Integer,
    iteration :: !Integer
  } deriving (Show)

initVars = ProcVars empty 0 0

newtype St s a = St { runSt :: s -> (a, s) }

instance Functor (St s) where
  fmap f m = St $ \s -> let
                (a, s') = runSt m s
                in (f a, s')

instance Monad(St s) where
  return = returnSt
  (>>=) = bindSt

instance MonadFix (St s) where
  mfix f = St $ \s -> let (a, s') = runSt (f a) s in (a, s')

instance Show( St s a) where
  show = showSt

showSt :: St s a -> String
showSt s = "St instance"

returnSt :: a -> St s a
returnSt a = St $ \s -> (a, s)

bindSt :: St s a -> (a -> St s b) -> St s b
bindSt m k = St $ \s -> let (a, s') = runSt m s in runSt (k a) s'

getSt :: St s s
getSt = St $ \s -> (s, s)

putSt :: s -> St s ()
putSt s = St $ \_ -> ((), s)
