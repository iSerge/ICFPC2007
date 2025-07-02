module TestDNA (testDNA) where

import Control.Monad.State.Strict
import DNAProcessor
import DNASeq
import qualified Data.Sequence as S
import ProcessorState

testDNA :: IO ()
testDNA = do
  putStr "Testing pattern processing:\n"
  putStr "  Testing nat(): "
  putStr "Skipped\n"
  putStr "  Testing pattern()\n"
  testPattern "IIPIFFCPICICIICPIICCFP" (S.fromList [LParen, Search (fromString "IFPP"), RParen, Pi baseF]) -- "(?[IFPP])F"
  testPattern "IIPIPCCCIIFFIPIICIIPCFPICIIFIICCC" (S.fromList [LParen, Skip 7, RParen, LParen, Pi baseI, Pi baseC, Pi baseF, Pi baseP, RParen]) -- "(!7)(ICFP)"
  testPattern "IIPIFCCPICICIICPIIPCFIIFIICIPIFPIFPICCFPICIIPCPIIFCCCIFPPFICFP" (S.fromList [LParen, Search (fromString "IFPP"), RParen, Pi baseF, LParen, Pi baseI, Pi baseC, RParen]) -- "(!7)(ICFP)"
  putStr "Testing matchreplace:\n"
  putStr "  Testing searchDNA()\n"
  testFailfn (fromString "ICFPICFP") [0, 0, 0, 0, 1, 2, 3, 4]
  testSearch (fromString "ICF") (fromString "PCFICFP") (Just 6)
  testSearch (fromString "ICF") (fromString "PCFIC") Nothing
  testSearch (fromString "ICF") (fromString "PCFICF") (Just 6)
  testSearch (fromString "ICF") (fromString "PICFCFICFP") (Just 4)
  testSearch (fromString "ICF") (fromString "PCFICPP") Nothing
  putStr "  Testing matchreplace()\n"
  testMatchreplace "IIPIFICPICICIICPIICIFIFPIFPICIICCCCIFPPFICFP" (fromString "CCCIFPPPICFP")
  testMatchreplace "IIPIFCCPICICIICPIICIPIFPIFPICIIFCCCIFPPFICFP" (fromString "CCCIFPPPICFP")
  testMatchreplace "IIPIFPCPICICIICPIIPCFIIFIICIPIFPIFPICCFPICIIPCPIIFCCCIFPPFICFP" (fromString "CCCIFPPPICFPICPFP")
  testPattern "IIPIPCCCPIPCPIIFIIPIFFCFPIIFIIFIIPCPIIPPICFPICFPPPPPICFF" (S.fromList [LParen, Skip 7, Skip 1, RParen, LParen, Search (fromString "ICF"), RParen])
  testTemplate "IIPCPIIPPIIFICFPICFPPPPPICFF" (S.fromList [LengthOf 1, LengthOf 0])
  testMatchreplace "IIPIPICPIICICIIFICCIFPPIICCFPC" (fromString "PICFC")
  testMatchreplace "IIPIPICPIICICIIFICCIFCCCPPIICCFPC" (fromString "PIICCFCFFPC")
  testMatchreplace "IIPIPIICPIICIICCIICFCFC" (fromString "I")
  testMatchreplace "IIPIPCCCPIPCPIIFIIPIFFCFPIIFIIFIIPCPIIPPIIFICFPICFPPPPPICFF" (fromString "CCCPIIICPF")

testPattern :: String -> Pattern -> IO ()
testPattern s r = do
  putStr ("    pattern " ++ s ++ " -> " ++ pat2String r ++ ", got: ")
  dnaSt <- initDnaProcessor (fromString s)
  (p, st) <- runStateT pattern dnaSt
  let dna' = getDna st
  if p == r
    then putStr (pat2String p ++ " - OK, DNA: \"" ++ dna2String dna' ++ "\"\n")
    else putStr (pat2String p ++ " - FAIL, DNA: \"" ++ dna2String dna' ++ "\"\n")

testTemplate :: String -> Template -> IO ()
testTemplate s r = do
  putStr ("    template " ++ s ++ " -> " ++ tpl2String r ++ ", got: ")
  dnaSt <- initDnaProcessor (fromString s)
  (t, st) <- runStateT template dnaSt
  let dna' = getDna st
  if t == r
    then putStr (tpl2String t ++ " - OK, DNA: \"" ++ dna2String dna' ++ "\"\n")
    else putStr (tpl2String t ++ " - FAIL, DNA: \"" ++ dna2String dna' ++ "\"\n")

testMatchreplace :: String -> DNA -> IO ()
testMatchreplace s r = do
  putStr ("    matchreplace " ++ s ++ " -> " ++ dna2String r ++ ", got: ")
  dnaSt <- initDnaProcessor (fromString s)
  st <- execStateT (do p<- pattern; t<-template; matchreplace p t ) dnaSt
  let dna''' = getDna st
  if dna''' == r
    then putStr (dna2String dna''' ++ " - OK\n")
    else putStr (dna2String dna''' ++ " - FAIL\n")

testSearch :: DNA -> DNA -> Maybe Integer -> IO ()
testSearch a b r = do
  putStr ("    searchDNA " ++ dna2String a ++ " " ++ dna2String b ++ " -> " ++ show r ++ ", got: ")
  if s == r
    then putStr (show s ++ " - OK\n")
    else putStr (show s ++ " - FAIL\n")
  where
    s = search a b

testFailfn d r = do
  putStr ("    Fail function of " ++ dna2String d ++ " is ")
  if s == r
    then putStr (show s ++ " - OK\n")
    else putStr (show s ++ " - FAIL\n")
  where
    s = runFf d $ failureFunc d

runFf d f = runFfIter f 0 (fromInteger $ len d) []

runFfIter f i 0 r = reverse r
runFfIter f i n r = runFfIter f (i + 1) (n - 1) (f i : r)
