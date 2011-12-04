module Main where

import DNA
import DNASeq
--import DNAByteStr
import DNAProcessor
import Control.Monad.State
import ProcessorState
import qualified Data.Sequence as S

main = test

test :: IO ()
test = do
  putStr "Testing pattern processing:\n"
  putStr "  Testing nat(): "
  putStr "Skipped\n"
  putStr "  Testing pattern()\n"
  testPattern "IIPIFFCPICICIICPIICCFP" (S.fromList [LParen, Search (fromString "IFPP"), RParen, Pi baseF]) --"(?[IFPP])F"
  testPattern "IIPIPCCCIIFFIPIICIIPCFPICIIFIICCC" (S.fromList [LParen, Skip 7, RParen, LParen, Pi baseI, Pi baseC, Pi baseF, Pi baseP, RParen]) --"(!7)(ICFP)"
  testPattern "IIPIFCCPICICIICPIIPCFIIFIICIPIFPIFPICCFPICIIPCPIIFCCCIFPPFICFP" (S.fromList [LParen, Search (fromString "IFPP"), RParen, Pi baseF ,LParen, Pi baseI, Pi baseC, RParen]) --"(!7)(ICFP)"
  putStr "Testing matchreplace:\n"
  putStr "  Testing searchDNA()\n"
  testFailfn (fromString "ICFPICFP") [0, 0, 0 ,0 ,1, 2, 3, 4]
  testSearch (fromString "ICF") (fromString "PCFICFP") (Just 6)
  testSearch (fromString "ICF") (fromString "PCFIC") Nothing
  testSearch (fromString "ICF") (fromString "PCFICF") (Just 6)
  testSearch (fromString "ICF") (fromString "PICFCFICFP") (Just 4)
  testSearch (fromString "ICF") (fromString "PCFICPP") Nothing
  putStr "  Testing matchreplace()\n"
  testMatchreplace "IIPIFICPICICIICPIICIFIFPIFPICIICCCCIFPPFICFP" (fromString "CCCIFPPPICFP")
  testMatchreplace "IIPIFCCPICICIICPIICIPIFPIFPICIIFCCCIFPPFICFP" (fromString "CCCIFPPPICFP")
  testMatchreplace "IIPIFPCPICICIICPIIPCFIIFIICIPIFPIFPICCFPICIIPCPIIFCCCIFPPFICFP" (fromString "CCCIFPPPICFPICPFP")
  testPattern "IIPIPCCCPIPCPIIFIIPIFFCFPIIFIIFIIPCPIIPPICFPICFPPPPPICFF" (S.fromList [LParen, Skip 7, Skip 1, RParen,LParen, Search (fromString "ICF"), RParen])
  testTemplate "IIPCPIIPPIIFICFPICFPPPPPICFF" (S.fromList [LengthOf 1, LengthOf 0])
  testMatchreplace "IIPIPCCCPIPCPIIFIIPIFFCFPIIFIIFIIPCPIIPPIIFICFPICFPPPPPICFF" (fromString "CCCPIIICPF")

--run_test :: (Eq a, Show a) => (String -> StateT ProcVars IO (a, ProcVars)) -> String -> a -> IO ()
--run_test f s r = do
--  putStr ("    pattern " ++ s ++ " -> " ++ (show r) ++ ", got: ")
--  ((p, st), v) <- runStateT (run_pattern s) initVars
--  case p == r of
--    True -> putStr ((show p) ++ " - OK, DNA: \"" ++ (dna2String $ dna st) ++ "\"\n")
--    False -> putStr ((show p) ++ " - FAIL, DNA: \"" ++ (dna2String $ dna st) ++ "\"\n")

testPattern :: String -> Pattern -> IO ()
testPattern s r = do
  putStr ("    pattern " ++ s ++ " -> " ++ pat2String r ++ ", got: ")
  ((p, st), v) <- runStateT (runPattern s) initVars
  if p == r
    then putStr (pat2String p ++ " - OK, DNA: \"" ++ dna2String (dna st) ++ "\"\n")
    else putStr (pat2String p ++ " - FAIL, DNA: \"" ++ dna2String (dna st) ++ "\"\n")

runPattern :: String -> StateT ProcVars IO (Pattern, ProcVars)
runPattern s = do
  addDNAPrefix s
  p <- pattern S.empty 0
  s <- get
  return (p, s)

testTemplate :: String -> Template -> IO ()
testTemplate s r = do
  putStr ("    template " ++ s ++ " -> " ++ tpl2String r ++ ", got: ")
  ((p, st), v) <- runStateT (runTemplate s) initVars
  if p == r
    then putStr (tpl2String p ++ " - OK, DNA: \"" ++ dna2String (dna st) ++ "\"\n")
    else putStr (tpl2String p ++ " - FAIL, DNA: \"" ++ dna2String (dna st) ++ "\"\n")

runTemplate :: String -> StateT ProcVars IO (Template, ProcVars)
runTemplate s = do
  addDNAPrefix s
  p <- template S.empty
  s <- get
  return (p, s)

testMatchreplace :: String -> DNA -> IO ()
testMatchreplace s r = do
  putStr ("    pattern " ++ s ++ " -> " ++ dna2String r ++ ", got: ")
  (d, v) <- runStateT (runMatchreplace s) initVars
  if d == r
    then putStr (dna2String d ++ " - OK\n")
    else putStr (dna2String d ++ " - FAIL\n")

runMatchreplace :: String -> StateT ProcVars IO DNA
runMatchreplace s = do
  addDNAPrefix s
  p <- pattern S.empty 0
  t <- template S.empty
  matchreplace p t
  s <- get
  return (dna s)

testSearch :: DNA -> DNA -> Maybe Integer -> IO ()
testSearch a b r = do
  putStr ("    searchDNA " ++ dna2String a ++ " " ++ dna2String b ++ " -> " ++ show r ++ ", got: ")
  if s == r
    then putStr (show s ++ " - OK\n")
    else putStr (show s ++ " - FAIL\n")
  where s = search a b

testFailfn :: DNA -> [Integer] -> IO ()
testFailfn d r = do
  putStr ("    Fail function of " ++ dna2String d ++ " is ")
  if s == r
    then putStr (show s ++ " - OK\n")
    else putStr (show s ++ " - FAIL\n")
  where s = runFf d $ failureFunc d

runFf :: DNA -> (Integer -> Integer) -> [Integer]
runFf d f = runFfIter f 0 (fromInteger  $len d) []

runFfIter :: (Integer -> Integer) -> Integer -> Integer -> [Integer] -> [Integer]
runFfIter f i 0 r = reverse r
runFfIter f i n r = runFfIter f (i + 1) (n - 1) (f i : r)

