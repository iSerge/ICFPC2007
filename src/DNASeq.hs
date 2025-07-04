{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DNASeq where

import Data.Array.IArray
import qualified Data.Foldable as F
import qualified Data.Sequence as S

data Base = I | C | F | P deriving (Eq, Ord, Show)

baseI :: Base
baseI = I

baseC :: Base
baseC = C

baseF :: Base
baseF = F

baseP :: Base
baseP = P

dnaI :: DNA
dnaI = singleton baseI

dnaC :: DNA
dnaC = singleton baseC

dnaF :: DNA
dnaF = singleton baseF

dnaP :: DNA
dnaP = singleton baseP

dnaIC :: DNA
dnaIC = dnaI S.|> baseC

dnaIP :: DNA
dnaIP = dnaI S.|> baseP

dnaIF :: DNA
dnaIF = dnaI S.|> baseF

dnaIIC :: DNA
dnaIIC = dnaI S.|> baseI S.|> baseC

dnaIIP :: DNA
dnaIIP = dnaI S.|> baseI S.|> baseP

dnaIIF :: DNA
dnaIIF = dnaI S.|> baseI S.|> baseF

dnaIII :: DNA
dnaIII = dnaI S.|> baseI S.|> baseI

type DNA = S.Seq Base

toDNA :: String -> DNA
toDNA = S.fromList . map char2Base

infixr 5 ><

infixr 5 <|

infixl 5 |>

fromString :: String -> DNA
fromString = toDNA

sect :: Integer -> Integer -> DNA -> DNA
sect m n = sectS (fromInteger m) (fromInteger n)

sectFrom :: Integer -> S.Seq a -> S.Seq a
sectFrom n = sectFromS (fromInteger n)

sectN :: Integer -> DNA -> Base
sectN n = sectNS (fromInteger n)

len :: DNA -> Integer
len = lenS

(<|) :: Base -> DNA -> DNA
(<|) = (S.<|)

(><) :: DNA -> DNA -> DNA
(><) = (S.><)

(|>) :: DNA -> Base -> DNA
(|>) = (S.|>)

emptyDNA :: DNA
emptyDNA = S.empty

isEmpty :: DNA -> Bool
isEmpty = S.null

isPrefix :: DNA -> DNA -> Bool
isPrefix = isPrefixSq

search :: DNA -> DNA -> Maybe Integer
search = fastSearchS

searchFrom :: Integer -> DNA -> DNA -> Maybe Integer
searchFrom i = fastSearchFromS $ fromInteger i

defrag :: a -> a
defrag = id

sectS :: Int -> Int -> S.Seq a -> S.Seq a
sectS m n = S.take (n - m) . S.drop m

sectFromS :: Int -> S.Seq a -> S.Seq a
sectFromS = S.drop

sectNS :: Int -> DNA -> Base
sectNS i d = S.index d i

lenS :: DNA -> Integer
lenS a = toInteger $ S.length a

singleton :: Base -> DNA
singleton = S.singleton

failureFunc :: DNA -> Int -> Int
failureFunc d = f
  where
    f t = case t of
      0 -> 0
      _ -> dna_iter d (f (t - 1)) (S.length d) t
        where
          dna_iter d t n i =
            if i < n
              then
                if t > 0 && (S.index d i /= S.index d t)
                  then dna_iter d (f (t - 1)) n i
                  else
                    if S.index d i == S.index d t
                      then t + 1
                      else 0
              else 0

fastSearchS :: DNA -> DNA -> Maybe Integer
fastSearchS a b = fastSearchIterS a b aLen bLen (array (0, aLen - 1) [(i, failureFunc a i) | i <- [0 .. aLen - 1]]) 0 0
  where
    aLen = S.length a
    bLen = S.length b

fastSearchFromS :: Int -> DNA -> DNA -> Maybe Integer
fastSearchFromS start a b = fastSearchIterS a (S.drop start b) aLen bLen (array (0, aLen - 1) [(i, failureFunc a i) | i <- [0 .. aLen - 1]]) start 0
  where
    aLen = S.length a
    bLen = S.length b

fastSearchIterS :: DNA -> DNA -> Int -> Int -> Array Int Int -> Int -> Int -> Maybe Integer
fastSearchIterS r d rlen dlen ff i s
  | s == rlen = Just $ toInteger i
  | S.null d = Nothing
  | i == dlen = Nothing
  | s > 0 && (S.index d 0 /= S.index r s) = fastSearchIterS r d rlen dlen ff i (ff ! (s - 1))
  | S.index d 0 == S.index r s = fastSearchIterS r (S.drop 1 d) rlen dlen ff (i + 1) (s + 1)
  | otherwise = fastSearchIterS r (S.drop 1 d) rlen dlen ff (i + 1) s

fastSearchIterS2 :: DNA -> DNA -> Int -> Int -> Array Int Int -> Int -> Int -> Maybe Integer
fastSearchIterS2 r d rlen dlen ff i s = fsi i s
  where
    fsi ii ss
      | ss == rlen = Just $ toInteger ii
      | ii == dlen = Nothing
      | ss > 0 && (S.index d ii /= S.index r ss) = fsi ii (ff ! (ss - 1))
      | S.index d ii == S.index r ss = fsi (ii + 1) (ss + 1)
      | otherwise = fsi (ii + 1) ss

isPrefixSq :: DNA -> DNA -> Bool
-- isPrefixSq a b = prefixIter a b 0 (S.length a) (S.length b)
isPrefixSq a b = a == S.take (S.length a) b

base2Char :: Base -> Char
base2Char I = 'I'
base2Char C = 'C'
base2Char F = 'F'
base2Char P = 'P'

char2Base :: Char -> Base
char2Base 'I' = I
char2Base 'C' = C
char2Base 'F' = F
char2Base 'P' = P

quote :: DNA -> DNA
quote = F.foldl' qFolder emptyDNA

qFolder :: DNA -> Base -> DNA
qFolder dna I = dna |> baseC
qFolder dna C = dna |> baseF
qFolder dna F = dna |> baseP
qFolder dna P = dna >< dnaIC

dna2String :: DNA -> String
dna2String = F.foldl' (\a b -> a ++ [base2Char b]) ""
