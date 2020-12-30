module Task2 where

  import Helpers (readLinesfromFile, countInString)
  import Data.Char (digitToInt)

  type Schema = ((Int, Int), Char, String)

  wordsToSchema :: [String] -> Schema
  wordsToSchema [range, charDef, password] =
    let char = head charDef
        [min, '-', max] = range
    in ((digitToInt min, digitToInt max), char, password) 

  verifySchema :: Schema -> Bool
  verifySchema ((min, max), char, password) =
    let count = countInString char password
    in count >= min && count <= max

  passwordFromSchema :: Schema -> String
  passwordFromSchema (_, _, password) = password
      
  main = do
    input <- readLinesfromFile "./data/2.txt"
    print
      $ map passwordFromSchema
      $ filter verifySchema
      $ map (wordsToSchema . words) input