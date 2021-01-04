module Helpers where

  toInt :: String -> Int
  toInt x = read x :: Int

  countInString :: Char -> String -> Int
  countInString char = length . filter (== char)

  readLinesfromFile :: FilePath -> IO [String]
  readLinesfromFile path = do
    input <- readFile path
    return (lines input)

  readNumbersFromFile :: FilePath -> IO [Int]
  readNumbersFromFile path = do
    linesFromFile <- readLinesfromFile path
    return (map toInt linesFromFile)
  