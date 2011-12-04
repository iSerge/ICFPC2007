module DNA where

import Prelude
import Data.Array.IArray

infixr 5 ><
infixr 5 <|
infixl 5 |>

class (Eq a) => DNAClass a where
  (|>) :: a -> a -> a
  (<|) :: a -> a -> a
  (><) :: a -> a -> a

  sect :: Integer -> Integer -> a -> a

  sectFrom :: Integer -> a -> a
  sectFrom a d = sect a (len d) d

  sectN :: Integer -> a -> a
  sectN a = sect a (a + 1)

  isPrefix :: a -> a -> Bool
  isPrefixS :: String -> a -> Bool
  isPrefixS = isPrefix . fromString
  fromString :: String -> a
  len :: a -> Integer
  defrag:: a -> a
  empty :: a
  isEmpty :: a -> Bool
  isEmpty a = a == empty
  search :: a -> a -> Maybe Integer
  search = fastSearch
  searchFrom :: Integer -> a -> a -> Maybe Integer
  searchFrom = fastSearchFrom
  --search a b = case searchIter a b (Just 0) of
  --  Just n -> Just (fromInteger n + len a)
  --  Nothing -> Nothing

--searchIter :: a -> a -> Maybe Integer -> Maybe Integer
--searchIter a b (Just n) | b /= empty = if isPrefix a b then Just n
--                                                      else searchIter a (sectFrom 1 b) (Just (n + 1))
--                        | otherwise = Nothing
--searchIter a b Nothing = Nothing

--  fromFile :: FilePath -> IO a

failureFunc :: (DNAClass a) => a -> Integer -> Integer
failureFunc d = f
 where
  f t = case t of
    0 -> 0
    _ -> dna_iter d (f (t - 1)) (len d) t
      where dna_iter d t n i = if i < n
                                    then if t > 0 && (sectN i d /= sectN t d)
                                      then dna_iter d (f (t - 1)) n i
                                      else if sectN i d == sectN t d
                                        then t + 1
                                        else 0
                                    else 0

fastSearch :: (DNAClass a) => a -> a -> Maybe Integer
fastSearch a b = fastSearchIter2 a b aLen bLen (array (0, aLen - 1) [(i, failureFunc a i) | i <- [0..aLen - 1]]) 0 0
    where aLen = len a
          bLen = len b

fastSearchFrom :: (DNAClass a) => Integer -> a -> a -> Maybe Integer
fastSearchFrom start a b = fastSearchIter2 a b aLen bLen (array (0, aLen - 1) [(i, failureFunc a i) | i <- [0..aLen - 1]]) start 0
    where aLen = len a
          bLen = len b

fastSearchIter :: (DNAClass a) => a -> a -> Integer -> Integer -> Array Integer Integer -> Integer -> Integer -> Maybe Integer
fastSearchIter r d rlen dlen ff i s
    | s == rlen                        = Just i
    | i == dlen                        = Nothing
    | s > 0 && (sectN i d /= sectN s r) = fastSearchIter r d rlen dlen ff i (ff ! (s-1))
    | sectN i d == sectN s r           = fastSearchIter r d rlen dlen ff (i + 1) (s + 1)
    | otherwise                       = fastSearchIter r d rlen dlen ff (i + 1) s

fastSearchIter2 :: (DNAClass a) => a -> a -> Integer -> Integer -> Array Integer Integer -> Integer -> Integer -> Maybe Integer
fastSearchIter2 r d rlen dlen ff i s = fsi i s
  where fsi ii ss = if ss == rlen then Just ii
                        else if ii == dlen then Nothing
                               else if ss > 0 && (sectN ii d /= sectN ss r) then fsi ii (ff ! (ss-1))
                                      else if sectN ii d == sectN ss r then fsi (ii + 1) (ss + 1)
                                                                          else fsi (ii + 1) ss
