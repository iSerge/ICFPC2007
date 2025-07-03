module DNAops (repairManualPage) where

import DNASeq

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
