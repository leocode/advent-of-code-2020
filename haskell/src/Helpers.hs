module Helpers where

  toInt :: String -> Int
  toInt x = read x :: Int

  toTuple :: [a] -> (a, a)
  toTuple [x, y] = (x, y)

  readNumbersFromFile :: FilePath -> IO [Int]
  readNumbersFromFile path = do
    input <- readFile path
    return (map toInt $ lines input)
