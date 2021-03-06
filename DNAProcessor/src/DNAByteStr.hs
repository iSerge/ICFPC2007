module DNAByteStr where

import DNA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Array.IArray
import Data.Int
import Data.Word

type Base = Char
baseI = 'I'
baseC = 'C'
baseF = 'F'
baseP = 'P'

dnaI = singleton baseI
dnaC = singleton baseC
dnaF = singleton baseF
dnaP = singleton baseP
dnaIC = B.cons' baseI $ singleton baseC
dnaIP = B.cons' baseI $ singleton baseP
dnaIF = B.cons' baseI $ singleton baseF
dnaIIC = singleton baseI `B.snoc` baseI `B.snoc` baseC
dnaIIP = singleton baseI `B.snoc` baseI `B.snoc` baseP
dnaIIF = singleton baseI `B.snoc` baseI `B.snoc` baseF
dnaIII = singleton baseI `B.snoc` baseI `B.snoc` baseI

type DNA = B.ByteString

base2Char :: Base -> Char
base2Char a = a

dna2String :: DNA -> String
dna2String = B.unpack

instance DNAClass B.ByteString where
  (<|) a = B.cons (B.head a)
  (|>) a b = B.snoc a (B.head b)
  (><) = B.append
  sect m n = sectBS (fromNum m) (fromNum n)
  sectFrom n = sectFromBS (fromNum n)
  sectN n = sectNBS (fromNum n)
  isPrefix = B.isPrefixOf
  len = lenBS
  empty = B.empty
  isEmpty = B.null
  search = fastSearchBS
  searchFrom = fastSearchFromBS

  fromString = fromStringBS
  defrag = B.copy
  --defrag = id

singleton :: Base -> DNA
singleton a = fromString [a]

sectBS m n = B.take (n - m) . B.drop m
sectFromBS = B.drop
--sectNBS n = B.take 1 . (B.drop n)
sectNBS i d = B.singleton (B.index d i)

fromNum :: (Num a, Integral a) => a -> Int64
fromNum a = fromInteger(toInteger a)

lenBS :: B.ByteString -> Integer
lenBS = toInteger . B.length

fromStringBS :: String -> DNA
fromStringBS = B.pack

fastSearchBS :: DNA -> DNA -> Maybe Integer
fastSearchBS a b = fastSearchIterBS2 a b aLen bLen (array (0, aLen - 1) [(i, fromInteger $ failureFunc a $ toInteger i) | i <- [0..aLen - 1]]) 0 0
    where aLen = B.length a
          bLen = B.length b

fastSearchFromBS :: Integer -> DNA -> DNA -> Maybe Integer
fastSearchFromBS start a b = fastSearchIterBS2 a b aLen bLen (array (0, aLen - 1) [(i, fromInteger $ failureFunc a $ toInteger i) | i <- [0..aLen - 1]]) (fromInteger start) 0
    where aLen = B.length a
          bLen = B.length b

fastSearchIterBS :: DNA -> DNA -> Int64 -> Int64 -> Array Int64 Int64 -> Int64 -> Int64 -> Maybe Integer
fastSearchIterBS r d rlen dlen ff i s
    | s == rlen                            = Just $ toInteger i
    | i == dlen                            = Nothing
    | s > 0 && (B.index d i /= B.index r s) = fastSearchIterBS r d rlen dlen ff i (ff ! (s-1))
    | B.index d i == B.index r s           = fastSearchIterBS r d rlen dlen ff (i + 1) (s + 1)
    | otherwise                           = fastSearchIterBS r d rlen dlen ff (i + 1) s

fastSearchIterBS2 :: DNA -> DNA -> Int64 -> Int64 -> Array Int64 Int64 -> Int64 -> Int64 -> Maybe Integer
fastSearchIterBS2 r d rlen dlen ff i s = fsi i s
  where fsi ii ss = if ss == rlen then Just $ toInteger ii
                        else if ii == dlen then Nothing
                               else if ss > 0 && (B.index d ii /= B.index r ss) then fsi ii (ff ! (ss-1))
                                      else if B.index d ii == B.index r ss then fsi (ii + 1) (ss + 1)
                                                                          else fsi (ii + 1) ss
