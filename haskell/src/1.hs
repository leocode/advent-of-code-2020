module Task1 where

  import Combinatorics (tuples)
  import Helpers (toInt, toTuple, readNumbersFromFile)

  -- | Extended 'tuples' function, which returns typed tuple
  tuples2 :: [a] -> [(a, a)]
  tuples2 = map toTuple . tuples 2

  isTupleSum :: (Eq a, Num a) => a -> (a, a) -> Bool
  isTupleSum desiredSum (v1, v2) = v1 + v2 == desiredSum

  findSums :: Int -> [Int] -> [(Int, Int)]
  findSums desiredSum = filter (isTupleSum desiredSum) . tuples2

  multiplyTuple :: (Int, Int) -> Int
  multiplyTuple (x, y) = x * y

  solution :: [Int] -> [Int]
  solution = map multiplyTuple . findSums 2020

  main = do
    numbers <- readNumbersFromFile "./data/1.txt"
    print $ solution numbers
    