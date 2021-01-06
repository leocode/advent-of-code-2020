module Task1 where

  import Combinatorics (tuples)
  import Helpers (toInt, readNumbersFromFile)

  findSums :: Int -> [[Int]] -> [[Int]]
  findSums desiredSum = filter (\tuple -> sum tuple == desiredSum)

  solution :: Int -> [Int] -> [Int]
  solution tupleSize = map product . findSums 2020 . tuples tupleSize

  main = do
    numbers <- readNumbersFromFile "./data/1.txt"
    print $ solution 2 numbers
    print $ solution 3 numbers
    