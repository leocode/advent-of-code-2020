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
  
  xor :: Bool -> Bool -> Bool
  xor a b = a /= b

  charAt :: Int -> String -> Maybe Char
  charAt _ [] = Nothing
  charAt 0 (x:xs) = Just x
  charAt pos (x:xs) = charAt (pos - 1) xs

  maybeToBool :: Maybe Char -> Bool
  maybeToBool (Just _) = True
  maybeToBool Nothing = False