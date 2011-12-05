module DNAProcessor where

import Prelude hiding (drop, reverse, length)
import qualified Data.Foldable as F
import DNASeq
--import qualified Data.ByteString.Lazy.Char8 as B
--import DNAByteStr
import Data.Array.IArray
import qualified Data.Sequence as S
import ProcessorState
import Control.Monad
import Control.Monad.State
import System.IO
import System.IO.Unsafe

type Environment = S.Seq DNA


nat :: StateT ProcVars IO Integer
nat = do s <- get
         let (dna', result) = natIter 0 0 (dna s)
         put s{dna = dna'}
         return result
{-
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
-}

--powers :: (IArray a e) => a Integer Integer
--powers = array (0, 63) [(i, 2^i) | i <- [0..63]]

natR = natIter 0 0

natIter :: Integer -> Integer -> DNA -> (DNA, Integer)
natIter n result dna | sectN 0 dna == baseP                         = (sectFrom 1 dna, result)
                     | sectN 0 dna == baseC                         = natIter (n+1) (result + 2^n) (sectFrom 1 dna)
                     | sectN 0 dna == baseI || sectN 0 dna == baseF = natIter (n+1) result (sectFrom 1 dna)

consts :: StateT ProcVars IO DNA
consts = do s <- get
            if isPrefix dnaC $ dna s
              then do
                consumeDNA 1
                b <- consts
                return (baseI <| b)
              else if isPrefix dnaF $ dna s
                then do
                  consumeDNA 1
                  b <- consts
                  return (baseC <| b)
                else if isPrefix dnaP $ dna s
                  then do
                    consumeDNA 1
                    b <- consts
                    return (baseF <| b)
                  else if isPrefix dnaIC $ dna s
                    then do
                      consumeDNA 2
                      b <- consts
                      return (baseP <| b)
                    else return empty

constsR = constsIter empty

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

patternR = patternIter 0 S.empty

patternIter :: Integer -> Pattern -> DNA -> IO (DNA, Pattern)
patternIter l p dna | isPrefix dnaC dna   = patternIter l (p S.|> Pi baseI) (sectFrom 1 dna)
                    | isPrefix dnaF dna   = patternIter l (p S.|> Pi baseC) (sectFrom 1 dna)
                    | isPrefix dnaP dna   = patternIter l (p S.|> Pi baseF) (sectFrom 1 dna)
                    | isPrefix dnaIC dna  = patternIter l (p S.|> Pi baseP) (sectFrom 2 dna)
                    | isPrefix dnaIP dna  = let (dna', n) = natR (sectFrom 2 dna) in patternIter l (p S.|> Skip n) dna'
                    | isPrefix dnaIF dna  = let (dna', s) = constsR (sectFrom 3 dna) in patternIter l (p S.|> Search s) dna'
                    | isPrefix dnaIIP dna = patternIter (l+1) (p S.|> LParen) (sectFrom 3 dna)
                    | isPrefix dnaIIC dna || isPrefix dnaIIF dna = if l == 0
                             then return ((sectFrom 3 dna), p)
                             else patternIter (l-1) (p S.|> RParen) (sectFrom 3 dna)
                    | isPrefix dnaIII dna = patternIter l p (sectFrom 10 dna)
                    | otherwise           = error "pattern: finish() - 1"

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

templateR = templateIter S.empty

templateIter :: Template -> DNA -> IO (DNA, Template)
templateIter t dna | isPrefix dnaC dna   = templateIter (t S.|> Ti baseI) (sectFrom 1 dna)
                   | isPrefix dnaF dna   = templateIter (t S.|> Ti baseC) (sectFrom 1 dna)
                   | isPrefix dnaP dna   = templateIter (t S.|> Ti baseF) (sectFrom 1 dna)
                   | isPrefix dnaIC dna  = templateIter (t S.|> Ti baseP) (sectFrom 2 dna)
                   | isPrefix dnaIF dna || isPrefix dnaIP dna = let (dna', l) = natR (sectFrom 2 dna)
                                                                    (dna'', n) = natR dna'
                                                                         in templateIter (t S.|> Protected n l) dna''
                   | isPrefix dnaIIP dna = let (dna', n) = natR (sectFrom 3 dna) in templateIter (t S.|> LengthOf n) dna'
                   | isPrefix dnaIIC dna || isPrefix dnaIIF dna = return ((sectFrom 3 dna), t)
                   | isPrefix dnaIII dna = templateIter t (sectFrom 10 dna)
                   | otherwise           = error "template: finish() - 1"

type MatcherSt = Maybe (Integer, Environment, S.Seq Integer)

matchreplace :: Pattern -> Template -> StateT ProcVars IO ()
matchreplace p t = do
  s <- get
  let d = dna s in
    case F.foldl' (matchMapper (d, len d)) (Just (0, S.empty, S.empty)) p of
      Just (i, e, _) -> do
          consumeDNA i
          replace t e
      Nothing -> return ()

matchreplaceR :: Pattern -> Template -> DNA -> IO DNA
matchreplaceR p t dna = case F.foldl' (matchMapper (dna, len dna)) (Just (0, S.empty, S.empty)) p of
      Just (i, e, _) -> replaceR t e (sectFrom i dna)
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

replace :: Template -> Environment -> StateT ProcVars IO ()
replace t e = do
  s <- get
  put s{dna = (F.foldl' (replMapper e) empty t) >< dna s}
  --put s{dna = defrag (F.foldl' (replMapper e) empty t) >< dna s}

replaceR :: Template -> Environment -> DNA -> IO DNA
replaceR t e = return . (F.foldl' (replMapper e) empty t ><)

replMapper :: Environment -> DNA -> TItem -> DNA
replMapper _ d (Ti b)          = d |> b
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

storeRNA :: StateT ProcVars IO ()
storeRNA = do s <- get
              io $ putStr $ dna2String (sect 3 10 (dna s))
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
  trace ("iteration: " ++ show (iteration s)) $ return ()

addDNAPrefix :: String -> StateT ProcVars IO ()
addDNAPrefix p = do
  s <- get
  put s{dna = fromString p >< dna s}

exec :: String -> StateT ProcVars IO ()
exec dna = do
  s<- get
  put s{dna = fromString dna}
  execIter

execIter = do
  startIteration
  --p <- trace "parsing pattern" $ pattern S.empty 0
  p <- pattern S.empty 0
  t <- trace ("pattern: " ++ pat2String p) $ template S.empty
--  t <- template S.empty
  trace ("template: " ++ tpl2String t) $ matchreplace p t
--  matchreplace p t
  s <- get
  --if iteration s > 60 then trace "premature exit" $ return ()
  --                    else trace "cost" $ trace (cost s) $ execIter
--  trace "cost" $ trace (cost s) $ execIter
  --when (iteration s <= 15) execIter
  execIter
--  s <- get
--  put s{pat = p, tpl = t}

execR = execIterR 0 . fromString

execIterR i dna = do
    hSetBuffering stderr NoBuffering
    hPutStrLn stderr ("iteration: " ++ show i)
    (dna', p)  <- patternR dna
    --hPutStrLn stderr ("pattern: " ++ pat2String p)
    (dna'', t) <- templateR dna'
    --hPutStrLn stderr ("template: " ++ tpl2String t)
    dna'''     <- matchreplaceR p t dna''
    when (i <= 2000) $ execIterR (i+1) dna'''
    --execIterR (i+1) dna'''
