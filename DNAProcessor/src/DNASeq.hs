{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, BangPatterns #-}

module DNASeq where

import DNA
--import Prelude hiding (drop, take, length, foldr, foldl)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Array.IArray
--import Debug.Trace

data Base = I | C | F | P deriving (Eq, Ord, Show)
baseI = I
baseC = C
baseF = F
baseP = P

dnaI = singleton baseI
dnaC = singleton baseC
dnaF = singleton baseF
dnaP = singleton baseP
dnaIC = singleton baseI S.|> baseC
dnaIP = singleton baseI S.|> baseP
dnaIF = singleton baseI S.|> baseF
dnaIIC = singleton baseI S.|> baseI S.|> baseC
dnaIIP = singleton baseI S.|> baseI S.|> baseP
dnaIIF = singleton baseI S.|> baseI S.|> baseF
dnaIII = singleton baseI S.|> baseI S.|> baseI

type DNA = S.Seq Base

data RNAOp = RNAOp Base Base Base Base Base Base Base deriving (Eq, Ord)

toDNA :: String -> DNA
toDNA = F.foldl' (flip $ flip (S.|>) . char2Base) empty

instance DNAClass DNA where
  fromString = toDNA
  sect m n = sectS (fromInteger m) (fromInteger n)
  sectFrom n = sectFromS (fromInteger n)
  sectN n = sectNS (fromInteger n)
  len = lenS
  (<|) a = (S.<|) (S.index a 0)
  (><) = (S.><)
  (|>) a b = (S.|>) a (S.index b 0)
  empty = S.empty
  isPrefix = isPrefixSq
  search = fastSearchS
  searchFrom i = fastSearchFromS $ fromInteger i
  defrag = id

sectS m n = S.take (n - m) . S.drop m
sectFromS = S.drop

sectNS :: Int -> DNA -> DNA
sectNS i d = S.singleton (S.index d i)  --S.take 1 . (S.drop n)

lenS :: DNA -> Integer
lenS a = toInteger $ S.length a

singleton :: Base -> DNA
singleton = S.singleton

fastSearchS :: DNA -> DNA -> Maybe Integer
fastSearchS a b = fastSearchIterS a b aLen bLen (array (0, aLen - 1) [(i, fromInteger $ failureFunc a $ toInteger i) | i <- [0..aLen - 1]]) 0 0
    where aLen = S.length a
          bLen = S.length b

fastSearchFromS :: Int -> DNA -> DNA -> Maybe Integer
fastSearchFromS !start !a !b = fastSearchIterS a (S.drop start b) aLen bLen (array (0, aLen - 1) [(i, fromInteger $ failureFunc a $ toInteger i) | i <- [0..aLen - 1]]) start 0
    where aLen = S.length a
          bLen = S.length b

fastSearchIterS :: DNA -> DNA -> Int -> Int -> Array Int Int -> Int -> Int -> Maybe Integer
fastSearchIterS r d rlen dlen ff i s
    | s == rlen                            = Just $ toInteger i
    | S.null d                            = Nothing
    | i == dlen                            = Nothing
    | s > 0 && (S.index d 0 /= S.index r s) = fastSearchIterS r d rlen dlen ff i (ff ! (s-1))
    | S.index d 0 == S.index r s           = fastSearchIterS r (S.drop 1 d) rlen dlen ff (i + 1) (s + 1)
    | otherwise                           = fastSearchIterS r (S.drop 1 d) rlen dlen ff (i + 1) s

{-
fastSearchIterS :: DNA -> DNA -> Int -> Int -> Array Int Int -> Int -> Int -> Maybe Integer
fastSearchIterS r d rlen dlen ff i s
    | s == rlen                            = Just $ toInteger i
    | i == dlen                            = Nothing
    | s > 0 && (S.index d i /= S.index r s) = fastSearchIterS r d rlen dlen ff i (ff ! (s-1))
    | S.index d i == S.index r s           = fastSearchIterS r d rlen dlen ff (i + 1) (s + 1)
    | otherwise                           = fastSearchIterS r d rlen dlen ff (i + 1) s
-}

fastSearchIterS2 :: DNA -> DNA -> Int -> Int -> Array Int Int -> Int -> Int -> Maybe Integer
fastSearchIterS2 r d rlen dlen ff i s = fsi i s
  where fsi ii ss = if ss == rlen then Just $ toInteger ii
                        else if ii == dlen then Nothing
                               else if ss > 0 && (S.index d ii /= S.index r ss) then fsi ii (ff ! (ss-1))
                                      else if S.index d ii == S.index r ss then fsi (ii + 1) (ss + 1)
                                                                          else fsi (ii + 1) ss

isPrefixSq :: DNA -> DNA -> Bool
--isPrefixSq a b = prefixIter a b 0 (S.length a) (S.length b)
isPrefixSq a b = a == S.take (S.length a) b

{-
prefixIter :: DNA -> DNA -> Int ->  Int -> Int -> Bool
prefixIter a b i la lb = if la > i
    then if lb > i && S.index a i == S.index b i
        then prefixIter a b (i + 1) la lb
        else False
    else True
-}

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

dna2String :: DNA -> String
dna2String = F.foldl' (\a b -> a ++ [base2Char b]) ""
