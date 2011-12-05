module Main where

import DNAProcessor
import Control.Monad.State
import ProcessorState
import System.Environment
import System.IO

processDNA :: String -> String -> IO ()
processDNA prefix dna = do runStateT (exec (prefix ++ dna)) initVars
                           return ()

main :: IO ()
main = do args <- getArgs
          dna <- readFile $ head args
          let prefix = if length args > 1 then head $ tail args else ""
          processDNA prefix dna
