module DNAProcessor where

import Prelude hiding (drop, reverse, length)
import qualified Data.Foldable as F
import DNA
import Data.Array.IArray
import qualified Data.Sequence as S
import ProcessorState
import Control.Monad
import Control.Monad.State
import System.IO
import System.IO.Unsafe

type Environment = S.Seq DNA

--powers :: (IArray a e) => a Integer Integer
--powers = array (0, 63) [(i, 2^i) | i <- [0..63]]

nat = natIter 0 0

natIter :: Integer -> Integer -> DNA -> (DNA, Integer)
natIter n result dna | sectN 0 dna == baseP                         = (sectFrom 1 dna, result)
                     | sectN 0 dna == baseC                         = natIter (n+1) (result + 2^n) (sectFrom 1 dna)
                     | sectN 0 dna == baseI || sectN 0 dna == baseF = natIter (n+1) result (sectFrom 1 dna)

consts = constsIter empty

constsIter :: DNA -> DNA -> (DNA, DNA)
constsIter r dna | isPrefix dnaC dna  = constsIter (r |> baseI) (sectFrom 1 dna)
                 | isPrefix dnaF dna  = constsIter (r |> baseC) (sectFrom 1 dna)
                 | isPrefix dnaP dna  = constsIter (r |> baseF) (sectFrom 1 dna)
                 | isPrefix dnaIC dna = constsIter (r |> baseP) (sectFrom 2 dna)
                 | otherwise          = (dna, r)

protect :: Integer -> DNA -> DNA
protect 0 x = defrag x
protect n x = protect (n - 1) $ quote x

asnat :: Integer -> DNA
asnat i = asnatIter i empty

asnatIter 0 dna = dna |> baseP
asnatIter i dna = if even i
    then asnatIter (i  `div` 2) (dna |> baseI)
    else asnatIter (i  `div` 2) (dna |> baseC)

pattern = patternIter 0 S.empty

patternIter :: Integer -> Pattern -> DNA -> IO (DNA, Pattern)
patternIter l p dna | isPrefix dnaC dna   = patternIter l (p S.|> Pi baseI) (sectFrom 1 dna)
                    | isPrefix dnaF dna   = patternIter l (p S.|> Pi baseC) (sectFrom 1 dna)
                    | isPrefix dnaP dna   = patternIter l (p S.|> Pi baseF) (sectFrom 1 dna)
                    | isPrefix dnaIC dna  = patternIter l (p S.|> Pi baseP) (sectFrom 2 dna)
                    | isPrefix dnaIP dna  = let (dna', n) = nat (sectFrom 2 dna) in patternIter l (p S.|> Skip n) dna'
                    | isPrefix dnaIF dna  = let (dna', s) = consts (sectFrom 3 dna) in patternIter l (p S.|> Search s) dna'
                    | isPrefix dnaIIP dna = patternIter (l+1) (p S.|> LParen) (sectFrom 3 dna)
                    | isPrefix dnaIIC dna || isPrefix dnaIIF dna = if l == 0
                             then return ((sectFrom 3 dna), p)
                             else patternIter (l-1) (p S.|> RParen) (sectFrom 3 dna)
                    | isPrefix dnaIII dna = putStr (dna2String $ sect 3 10 dna) >> patternIter l p (sectFrom 10 dna)
                    | otherwise           = error "pattern: finish() - 1"

template = templateIter S.empty

templateIter :: Template -> DNA -> IO (DNA, Template)
templateIter t dna | isPrefix dnaC dna   = templateIter (t S.|> Ti baseI) (sectFrom 1 dna)
                   | isPrefix dnaF dna   = templateIter (t S.|> Ti baseC) (sectFrom 1 dna)
                   | isPrefix dnaP dna   = templateIter (t S.|> Ti baseF) (sectFrom 1 dna)
                   | isPrefix dnaIC dna  = templateIter (t S.|> Ti baseP) (sectFrom 2 dna)
                   | isPrefix dnaIF dna || isPrefix dnaIP dna = let (dna', l) = nat (sectFrom 2 dna)
                                                                    (dna'', n) = nat dna'
                                                                         in templateIter (t S.|> Protected n l) dna''
                   | isPrefix dnaIIP dna = let (dna', n) = nat (sectFrom 3 dna) in templateIter (t S.|> LengthOf n) dna'
                   | isPrefix dnaIIC dna || isPrefix dnaIIF dna = return ((sectFrom 3 dna), t)
                   | isPrefix dnaIII dna = putStr (dna2String $ sect 3 10 dna) >> templateIter t (sectFrom 10 dna)
                   | otherwise           = error "template: finish() - 1"

type MatcherSt = Maybe (Integer, Environment, S.Seq Integer)

matchreplace :: Pattern -> Template -> DNA -> IO DNA
matchreplace p t dna = case F.foldl' (matchMapper (dna, len dna)) (Just (0, S.empty, S.empty)) p of
      Just (i, e, _) -> replace t e (sectFrom i dna)
      Nothing        -> return dna

matchMapper :: (DNA, Integer) -> MatcherSt -> PItem -> MatcherSt
matchMapper (d, ld) (Just (i, e, c)) (Pi b)     = if sectN i d == b then Just (i + 1, e, c)
                                                                             else Nothing
matchMapper (d, ld) (Just (i, e, c)) (Skip n)   = if ld >= (i + n) then Just (i + n, e, c)
                                                                  else Nothing
matchMapper (d, ld) (Just (i, e, c)) (Search s) = case searchFrom i s d of
                                                    Just n -> Just (n, e, c)
                                                    Nothing -> Nothing
matchMapper (d, ld) (Just (i, e, c)) LParen     = Just (i, e, i S.<| c)
matchMapper (d, ld) (Just (i, e, c)) RParen     = Just (i, e S.|> copyDNA c i d, S.drop 1 c)
matchMapper _ Nothing _                         = Nothing

copyDNA :: S.Seq Integer -> Integer -> DNA -> DNA
copyDNA c i d = case S.viewl c of
  S.EmptyL -> error "copyDNA: accessing empty index array"
  x S.:< xs -> sect x i d

replace :: Template -> Environment -> DNA -> IO DNA
replace t e = return . (F.foldl' (replMapper e) empty t ><)

replMapper :: Environment -> DNA -> TItem -> DNA
replMapper _ d (Ti b)          = d |> b
replMapper e d (Protected n l) = d >< protect l (getEnv n e)
replMapper e d (LengthOf n)    = d >< asnat (len $ getEnv n e)

getEnv :: Integer -> Environment -> DNA
getEnv n e | n < toInteger (S.length e) = S.index e $ fromInteger n
           | otherwise = empty

exec = execIter 0 . fromString

execIter i dna = do
    hSetBuffering stderr NoBuffering
    hPutStrLn stderr ("iteration: " ++ show i)
    (dna', p)  <- pattern dna
    --hPutStrLn stderr ("pattern: " ++ pat2String p)
    (dna'', t) <- template dna'
    --hPutStrLn stderr ("template: " ++ tpl2String t)
    dna'''     <- matchreplace p t dna''
    --when (i <= 2000) $ execIter (i+1) dna'''
    execIter (i+1) dna'''
