module DNAByteStr where

import Data.Array.IArray
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Int

type Base = Char

baseI = 'I'

baseC = 'C'

baseF = 'F'

baseP = 'P'

dnaI = singleton baseI

dnaC = singleton baseC

dnaF = singleton baseF

dnaP = singleton baseP

dnaIC = B.cons' baseI $ dnaC

dnaIP = B.cons' baseI $ dnaP

dnaIF = B.cons' baseI $ dnaF

dnaIIC = singleton baseI `B.snoc` baseI `B.snoc` baseC

dnaIIP = singleton baseI `B.snoc` baseI `B.snoc` baseP

dnaIIF = singleton baseI `B.snoc` baseI `B.snoc` baseF

dnaIII = singleton baseI `B.snoc` baseI `B.snoc` baseI

type DNA = B.ByteString

base2Char :: Base -> Char
base2Char a = a

dna2String :: DNA -> String
dna2String = B.unpack

(<|) a = B.cons a

(|>) a b = B.snoc a b

(><) = B.append

sect m n = sectBS (fromNum m) (fromNum n)

sectFrom n = sectFromBS (fromNum n)

sectN :: Integer -> DNA -> Base
sectN n = flip B.index $ fromInteger n

isPrefix = B.isPrefixOf

len = lenBS

empty = B.empty

isEmpty = B.null

search = fastSearchBS

searchFrom = fastSearchFromBS

fromString = fromStringBS

defrag = B.copy

singleton :: Base -> DNA
singleton a = fromString [a]

sectBS m n = B.take (n - m) . B.drop m

sectFromBS = B.drop

-- sectNBS n = B.take 1 . (B.drop n)
sectNBS i d = B.singleton (B.index d i)

fromNum :: (Num a, Integral a) => a -> Int64
fromNum a = fromInteger (toInteger a)

lenBS :: B.ByteString -> Integer
lenBS = toInteger . B.length

fromStringBS :: String -> DNA
fromStringBS = B.pack

failureFunc :: DNA -> Int64 -> Int64
failureFunc d = f
  where
    f t = case t of
      0 -> 0
      _ -> dna_iter d (f (t - 1)) (B.length d) t
        where
          dna_iter d t n i =
            if i < n
              then
                if t > 0 && (B.index d i /= B.index d t)
                  then dna_iter d (f (t - 1)) n i
                  else
                    if B.index d i == B.index d t
                      then t + 1
                      else 0
              else 0

fastSearchBS :: DNA -> DNA -> Maybe Integer
fastSearchBS a b = fastSearchIterBS2 a b aLen bLen (array (0, aLen - 1) [(i, failureFunc a i) | i <- [0 .. aLen - 1]]) 0 0
  where
    aLen = B.length a
    bLen = B.length b

fastSearchFromBS :: Integer -> DNA -> DNA -> Maybe Integer
fastSearchFromBS start a b = fastSearchIterBS2 a b aLen bLen (array (0, aLen - 1) [(i, failureFunc a i) | i <- [0 .. aLen - 1]]) (fromInteger start) 0
  where
    aLen = B.length a
    bLen = B.length b

fastSearchIterBS :: DNA -> DNA -> Int64 -> Int64 -> Array Int64 Int64 -> Int64 -> Int64 -> Maybe Integer
fastSearchIterBS r d rlen dlen ff i s
  | s == rlen = Just $ toInteger i
  | i == dlen = Nothing
  | s > 0 && (B.index d i /= B.index r s) = fastSearchIterBS r d rlen dlen ff i (ff ! (s - 1))
  | B.index d i == B.index r s = fastSearchIterBS r d rlen dlen ff (i + 1) (s + 1)
  | otherwise = fastSearchIterBS r d rlen dlen ff (i + 1) s

fastSearchIterBS2 :: DNA -> DNA -> Int64 -> Int64 -> Array Int64 Int64 -> Int64 -> Int64 -> Maybe Integer
fastSearchIterBS2 r d rlen dlen ff i s = fsi i s
  where
    fsi ii ss =
      if ss == rlen
        then Just $ toInteger ii
        else
          if ii == dlen
            then Nothing
            else
              if ss > 0 && (B.index d ii /= B.index r ss)
                then fsi ii (ff ! (ss - 1))
                else
                  if B.index d ii == B.index r ss
                    then fsi (ii + 1) (ss + 1)
                    else fsi (ii + 1) ss

quote :: DNA -> DNA
quote = B.foldl' qFolder empty

qFolder :: DNA -> Base -> DNA
qFolder dna 'I' = dna |> baseC
qFolder dna 'C' = dna |> baseF
qFolder dna 'F' = dna |> baseP
qFolder dna 'P' = dna >< dnaIC
