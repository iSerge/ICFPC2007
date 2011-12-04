{-# LANGUAGE BangPatterns #-}

module DNAProcessor where

import Prelude hiding (drop, reverse, length)
import qualified Data.Foldable as F
import DNA
import DNASeq
--import qualified Data.ByteString.Lazy.Char8 as B
--import DNAByteStr
import qualified Data.Sequence as S
import ProcessorState
import Control.Monad
import Control.Monad.State
import System.IO
import System.IO.Unsafe

type Environment = S.Seq DNA

nat :: StateT ProcVars IO Integer
nat = do s <- get
         if isPrefix dnaP (dna s)
           then do
                consumeDNA 1
                return 0
           else if isPrefix dnaC $ dna s
             then do
               consumeDNA 1
               n <- nat
               return (2 * n + 1)
             else if isPrefix dnaI (dna s) || isPrefix dnaF (dna s)
               then do
                 consumeDNA 1
                 n <- nat
                 return (2 * n)
             else error "nat - finish()"

consts :: StateT ProcVars IO DNA
consts = do s <- get
            if isPrefix dnaC $ dna s
              then do
                consumeDNA 1
                b <- consts
                return (singleton baseI <| b)
              else if isPrefix dnaF $ dna s
                then do
                  consumeDNA 1
                  b <- consts
                  return (singleton baseC <| b)
                else if isPrefix dnaP $ dna s
                  then do
                    consumeDNA 1
                    b <- consts
                    return (singleton baseF <| b)
                  else if isPrefix dnaIC $ dna s
                    then do
                      consumeDNA 2
                      b <- consts
                      return (singleton baseP <| b)
                    else return empty

quote :: DNA -> DNA
quote dna = quoteIter dna empty
--quote = B.foldl' (flip $ flip (><) . qMapper) empty

qMapper :: Base -> DNA
qMapper b | b == baseI = dnaC
          | b == baseC = dnaF
          | b == baseF = dnaP
          | b == baseP = dnaIC

quoteIter dna r | isPrefix dnaI dna = quoteIter (sectFrom 1 dna) (r |> dnaC)
                | isPrefix dnaC dna = quoteIter (sectFrom 1 dna) (r |> dnaF)
                | isPrefix dnaF dna = quoteIter (sectFrom 1 dna) (r |> dnaP)
                | isPrefix dnaP dna = quoteIter (sectFrom 1 dna) (r >< dnaIC)
                | otherwise = r

protect :: Integer -> DNA -> DNA
protect 0 x = defrag x
protect n x = protect (n - 1) $ quote x

asnat :: Integer -> DNA
asnat 0 = dnaP
asnat x = if even x
    then dnaI <| asnat (x `div` 2)
    else dnaC <| asnat (x `div` 2)

pattern :: Pattern -> Integer -> StateT ProcVars IO Pattern
pattern p l = do
  s <- get
  let dnaPref = sect 0 3 (dna s) in
   if isPrefix dnaC dnaPref
    then do
      consumeDNA 1
      pattern (p S.|> Pi baseI) l
    else if isPrefix dnaF dnaPref
      then do
        consumeDNA 1
        pattern (p S.|> Pi baseC) l
      else if isPrefix dnaP dnaPref
        then do
          consumeDNA 1
          pattern (p S.|> Pi baseF) l
        else if isPrefix dnaIC dnaPref
          then do
            consumeDNA 2
            pattern (p S.|> Pi baseP) l
          else if isPrefix dnaIP dnaPref
            then do
              consumeDNA 2
              n <- nat
              pattern (p S.|> Skip n) l
            else if isPrefix dnaIF dnaPref
              then do
                consumeDNA 3
                s <- consts
                pattern (p S.|> Search s) l
              else if isPrefix dnaIIP dnaPref
                then do
                  consumeDNA 3
                  pattern (p S.|> LParen) (l + 1)
                else if isPrefix dnaIIC dnaPref || isPrefix dnaIIF dnaPref
                  then do
                    consumeDNA 3
                    if l == 0
                        then return p
                        else pattern (p S.|> RParen) (l - 1)
                  else if isPrefix dnaIII dnaPref
                    then do
                      --consumeDNA 3
                      storeRNA
                      pattern p l
                    else error "pattern: finish() - 1"

template :: Template -> StateT ProcVars IO Template
template t = do
  s <- get
  let dnaPref = sect 0 3 (dna s) in
   if isPrefix dnaC dnaPref
    then do
      consumeDNA 1
      template (t S.|> Ti baseI)
    else if isPrefix dnaF dnaPref
      then do
        consumeDNA 1
        template (t S.|> Ti baseC)
      else if isPrefix dnaP dnaPref
        then do
          consumeDNA 1
          template (t S.|> Ti baseF)
        else if isPrefix dnaIC dnaPref
          then do
            consumeDNA 2
            template (t S.|> Ti baseP)
          else if isPrefix dnaIF dnaPref || isPrefix dnaIP dnaPref
            then do
              consumeDNA 2
              l <- nat
              n <- nat
              template (t S.|> Protected n l)
            else if isPrefix dnaIIC dnaPref || isPrefix dnaIIF dnaPref
              then do
                consumeDNA 3
                return t
              else if isPrefix dnaIIP dnaPref
                then do
                  consumeDNA 3
                  n <- nat
                  template (t S.|> LengthOf n)
                else if isPrefix dnaIII dnaPref
                  then do
                    --consumeDNA 3
                    storeRNA
                    template t
                  else error "template: finish() - 1"

type MatcherSt = Maybe (Integer, Environment, S.Seq Integer)

matchreplace :: Pattern -> Template -> StateT ProcVars IO ()
matchreplace !p !t = do
  s <- get
  let d = dna s in
    case F.foldl' (matchMapper (d, len d)) (Just (0, S.empty, S.empty)) p of
      Just (i, e, _) -> do
          consumeDNA i
          replace t e
      Nothing -> return ()

matchMapper :: (DNA, Integer) -> MatcherSt -> PItem -> MatcherSt
matchMapper (d, ld) (Just (i, e, c)) (Pi b)     = if sectN i d == singleton b then Just (i + 1, e, c)
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

replace :: Template -> Environment -> StateT ProcVars IO ()
replace t e = do
  s <- get
  put s{dna = (F.foldl' (replMapper e) empty t) >< dna s}
  --put s{dna = defrag (F.foldl' (replMapper e) empty t) >< dna s}

replMapper :: Environment -> DNA -> TItem -> DNA
replMapper _ d (Ti b)          = d |> singleton b
replMapper e d (Protected n l) = d >< protect l (getEnv n e)
replMapper e d (LengthOf n)    = d >< asnat (len $ getEnv n e)

getEnv :: Integer -> Environment -> DNA
getEnv n e | n < toInteger (S.length e) = S.index e $ fromInteger n
           | otherwise = empty

consumeDNA :: Integer -> StateT ProcVars IO ()
consumeDNA n = do s <- get
                  put s{dna = sectFrom n (dna s), cost = cost s + n}

addCost :: Integer -> StateT ProcVars IO ()
addCost n = do s <- get
               put s{cost = cost s + n}


readDNA :: String -> StateT ProcVars IO ()
readDNA f = do s <- get
               str <- io $ readFile f
               put (s{ dna = fromString str })

finish :: String -> StateT ProcVars IO Bool
finish e = do s <- get
              put s{fin = True}
              io $ putStr e
              return True

storeRNA :: StateT ProcVars IO ()
storeRNA = do s <- get
              io $ putStr $ dna2String (sect 3 10 (dna s))
              --put s{rna = (rna s) S.|> sect 0 7 (dna s)}
              consumeDNA 10

--trace :: (Show a) => a -> StateT ProcVars IO ()
--trace a = return $ unsafePerformIO $ hPutStr stderr (show a ++ "\n") --stderr (show a ++ "\n")
trace s f = unsafePerformIO $ do hSetBuffering stdout NoBuffering
                                 hSetBuffering stderr NoBuffering
                                 hPutStr stderr (show s ++ "\n")
                                 return f

io :: IO a -> StateT ProcVars IO a
io = liftIO

startIteration :: StateT ProcVars IO ()
startIteration = do
  s <- get
  put s{iteration = iteration s + 1}
  --when (iteration s `mod` 10 == 0) $ trace "iteration" $ trace (iteration s) $ return ()
  trace "iteration" $ trace (iteration s) $ return ()

addDNAPrefix :: String -> StateT ProcVars IO ()
addDNAPrefix p = do
  s <- get
  put s{dna = fromString p >< dna s}

exec :: String -> FilePath -> StateT ProcVars IO ()
exec p f= do
  readDNA f
  addDNAPrefix p
  execIter

execIter = do
  startIteration
--  p <- trace "parsing pattern" $ pattern S.empty 0
  p <- pattern S.empty 0
  t <- trace ("pattern: " ++ pat2String p) $ template S.empty
--  t <- template S.empty
  trace ("template: " ++ tpl2String t) $ matchreplace p t
--  matchreplace p t
  s <- get
  --if iteration s > 60 then trace "premature exit" $ return ()
  --                    else trace "cost" $ trace (cost s) $ execIter
--  trace "cost" $ trace (cost s) $ execIter
  --when (iteration s <= 100) execIter
  execIter
--  s <- get
--  put s{pat = p, tpl = t}

