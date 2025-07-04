{-# LANGUAGE BangPatterns #-}

module DNAProcessor
  ( dnaIter,
    initDnaProcessor,
    DnaProcSt (..),
    template,
    matchreplace,
    pattern,
  )
where

import Control.Monad
import Control.Monad.State.Strict
import DNASeq
import qualified Data.Sequence as S
import ProcessorState
import RNA
import System.IO
import Prelude hiding (drop, length, reverse)

data DnaProcSt = DnaProcSt
  { getRna :: !RNA,
    getDna :: !DNA,
    getRnaFull :: !(S.Seq String),
    dnaIteration :: !Integer
  }

initDnaProcessor :: DNA -> IO DnaProcSt
initDnaProcessor dna = return $ DnaProcSt {getDna = dna, getRna = emptyRNA, dnaIteration = 0, getRnaFull = S.empty}

type Environment = S.Seq DNA

emptyEnv :: Environment
emptyEnv = S.empty

nat :: (Integral a) => StateT DnaProcSt IO a
nat = natIter 0 0
  where
    natIter :: (Integral a) => a -> a -> StateT DnaProcSt IO a
    natIter n result = do
      dna <- gets getDna
      case dna of
        (P S.:<| dna') -> do putDna dna'; return result
        (C S.:<| dna') -> do putDna dna'; natIter (n + 1) (result + 2 ^ n)
        (I S.:<| dna') -> do putDna dna'; natIter (n + 1) result
        (F S.:<| dna') -> do putDna dna'; natIter (n + 1) result
        _ -> putDna emptyDNA >> return result

consts :: StateT DnaProcSt IO DNA
consts = constsIter emptyDNA
  where
    constsIter r = do
      dna <- gets getDna
      case dna of
        (C S.:<| dna') -> do putDna dna'; constsIter (r |> I)
        (F S.:<| dna') -> do putDna dna'; constsIter (r |> C)
        (P S.:<| dna') -> do putDna dna'; constsIter (r |> F)
        (I S.:<| C S.:<| dna') -> do putDna dna'; constsIter (r |> P)
        _ -> return r

protect :: Integer -> DNA -> DNA
protect 0 !x = defrag x
protect n !x = protect (n - 1) (quote x)

asnat :: Integer -> DNA
asnat i = asnatIter i emptyDNA
  where
    asnatIter 0 dna = dna |> P
    asnatIter j dna | even j = asnatIter (j `div` 2) (dna |> I)
    asnatIter j dna = asnatIter (j `div` 2) (dna |> C)

putDna :: DNA -> StateT DnaProcSt IO ()
putDna dna = modify (\s -> s {getDna = dna})

getRnaOp :: StateT DnaProcSt IO RNAop
getRnaOp = do
  dna <- gets getDna
  let rnaStr = dna2String $ sect 0 7 dna
  let op = parseRnaOp rnaStr
  modify (\s -> s {getRna = (getRna s) S.|> op, getRnaFull = (getRnaFull s) S.|> rnaStr})
  putDna $ sectFrom 7 dna
  return op

pattern :: StateT DnaProcSt IO Pattern
pattern = do
  patternIter 0 emptyPattern

patternIter :: Integer -> Pattern -> StateT DnaProcSt IO Pattern
patternIter l p = do
  dna <- gets getDna
  case dna of
    (C S.:<| dna') -> do putDna dna'; patternIter l (p S.|> Pi I)
    (F S.:<| dna') -> do putDna dna'; patternIter l (p S.|> Pi C)
    (P S.:<| dna') -> do putDna dna'; patternIter l (p S.|> Pi F)
    (I S.:<| C S.:<| dna') -> do putDna dna'; patternIter l (p S.|> Pi P)
    (I S.:<| P S.:<| dna') -> do putDna dna'; n <- nat; patternIter l (p S.|> Skip n)
    (I S.:<| F S.:<| _ S.:<| dna') -> do putDna dna'; s <- consts; patternIter l (p S.|> Search s)
    (I S.:<| I S.:<| P S.:<| dna') -> do putDna dna'; patternIter (l + 1) (p S.|> LParen)
    (I S.:<| I S.:<| C S.:<| dna') | l == 0 -> do putDna dna'; return p
    (I S.:<| I S.:<| C S.:<| dna') -> do putDna dna'; patternIter (l - 1) (p S.|> RParen)
    (I S.:<| I S.:<| F S.:<| dna') | l == 0 -> do putDna dna'; return p
    (I S.:<| I S.:<| F S.:<| dna') -> do putDna dna'; patternIter (l - 1) (p S.|> RParen)
    (I S.:<| I S.:<| I S.:<| dna') -> do putDna dna'; _ <- getRnaOp; patternIter l p
    _ -> putDna emptyDNA >> return p

template :: StateT DnaProcSt IO Template
template = templateIter emptyTemplate

templateIter :: Template -> StateT DnaProcSt IO Template
templateIter t = do
  dna <- gets getDna
  case dna of
    (C S.:<| dna') -> do putDna dna'; templateIter (t S.|> Ti I)
    (F S.:<| dna') -> do putDna dna'; templateIter (t S.|> Ti C)
    (P S.:<| dna') -> do putDna dna'; templateIter (t S.|> Ti F)
    (I S.:<| C S.:<| dna') -> do putDna dna'; templateIter (t S.|> Ti P)
    (I S.:<| F S.:<| dna') -> do putDna dna'; l <- nat; n <- nat; templateIter (t S.|> Protected n l)
    (I S.:<| P S.:<| dna') -> do putDna dna'; l <- nat; n <- nat; templateIter (t S.|> Protected n l)
    (I S.:<| I S.:<| C S.:<| dna') -> do putDna dna'; return t
    (I S.:<| I S.:<| F S.:<| dna') -> do putDna dna'; return t
    (I S.:<| I S.:<| P S.:<| dna') -> do putDna dna'; n <- nat; templateIter (t S.|> LengthOf n)
    (I S.:<| I S.:<| I S.:<| dna') -> do putDna dna'; _ <- getRnaOp; templateIter t
    _ -> putDna emptyDNA >> return t

type MatcherSt = Maybe (Integer, Environment, S.Seq Integer)

matchreplace :: Pattern -> Template -> StateT DnaProcSt IO ()
matchreplace p t = do
  dna <- gets getDna
  result <- foldM matchMapper (Just (0, emptyEnv, S.empty)) p
  case result of
    Just (i, e, _) -> do putDna (sectFrom i dna) >> replace t e
    Nothing -> return ()

matchMapper :: MatcherSt -> PItem -> StateT DnaProcSt IO MatcherSt
matchMapper (Just (i, e, c)) (Pi b) = do
  dna <- gets getDna
  if sectN i dna == b
    then return $ Just (i + 1, e, c)
    else return Nothing
matchMapper (Just (i, e, c)) (Skip n) = do
  dna <- gets getDna
  if fromIntegral (S.length dna) >= (i + n)
    then return $ Just (i + n, e, c)
    else return Nothing
matchMapper (Just (i, e, c)) (Search s) = do
  dna <- gets getDna
  case searchFrom i s dna of
    Just n -> return $ Just (n, e, c)
    Nothing -> return Nothing
matchMapper (Just (i, e, c)) LParen = return $ Just (i, e, i S.<| c)
matchMapper (Just (i, e, c)) RParen = do
  dna <- gets getDna
  return $ Just (i, e S.|> copyDNA c i dna, S.drop 1 c)
matchMapper Nothing _ = return Nothing

copyDNA :: S.Seq Integer -> Integer -> DNA -> DNA
copyDNA c i d = case S.viewl c of
  S.EmptyL -> error "copyDNA: accessing empty index array"
  x S.:< _ -> sect x i d

replace :: Template -> Environment -> StateT DnaProcSt IO ()
replace t e = do
  dna <- gets getDna
  prefix <- foldM (replMapper e) emptyDNA t
  putDna $ prefix >< dna

replMapper :: Environment -> DNA -> TItem -> StateT DnaProcSt IO DNA
replMapper _ d (Ti b) = return $ d |> b
replMapper e d (Protected n l) = return $ d >< protect l (getEnv n e)
replMapper e d (LengthOf n) = return $ d >< asnat (len $ getEnv n e)

getEnv :: Integer -> Environment -> DNA
getEnv n e
  | n < toInteger (S.length e) = S.index e $ fromInteger n
  | otherwise = emptyDNA

dnaIter :: StateT DnaProcSt IO RNA
dnaIter = do
  p <- pattern
  -- hPutStrLn stderr ("pattern: " ++ pat2String p)
  t <- template
  -- hPutStrLn stderr ("template: " ++ tpl2String t)
  matchreplace p t
  modify (\s -> s {dnaIteration = dnaIteration s + 1})
  gets getRna
