{-# LANGUAGE BangPatterns #-}

module DNAops (calcPrefix, repairManualPage) where

import DNASeq
import Text.Parsec

calcPrefix :: String -> IO String
calcPrefix p = do
  actions <- parseProg p
  let strictId !a = a
  foldMap strictId actions

parseProg :: String -> IO [IO String]
parseProg p = case parse program "" p of
  Left err -> print err >> return []
  Right xs -> return xs

program = sepEndBy expressions eol

expressions = choice [manualPage]

manualPage = do
  _ <- string "repairManualPage"
  spaces
  repairManualPage <$> integer

integer = do
  digits <- many $ oneOf "1234567890"
  return (read digits :: Int)

eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

nat :: Int -> String
nat n = natIter n ""
  where
    natIter 0 r = reverse r
    natIter i r = if even i then natIter (i `div` 2) ('I' : r) else natIter (i `div` 2) ('C' : r)

protect :: Int -> String -> String
protect 0 s = s
protect n s = protect (n - 1) $ foldMap subst s
  where
    subst 'I' = "C"
    subst 'C' = "F"
    subst 'F' = "P"
    subst 'P' = "IC"
    subst _ = ""

repairManualPage :: Int -> IO String
repairManualPage 0 = return "IIPIFFCPICFPPICIICCIICIPPPCIIC"
repairManualPage n = do
  let number = protect 1 $ nat n
      widthSpec = replicate (length number) 'C'
  return ("IIPIFFCPICFPPICIIC" ++ widthSpec ++ "IICIPPP" ++ number ++ "IIC")
