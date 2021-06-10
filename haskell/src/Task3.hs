module Task3 where

  import Helpers (readLinesfromFile)

  -- getPath builds infinite columns and passes function down to buildPath
  getPath :: [String] -> (Int, Int) -> String
  getPath rows (slopeX, slopeY) = 
    buildPath (map cycle rows)
    where
      -- 1. take first element (upper-left corner)
      -- 2. drop first slopeY rows and slopeX columns
      -- 3. call buildPath recursively with dropped input (so it again takes first element in corner)
      buildPath :: [String] -> String
      buildPath [] = []
      buildPath rows' =
        firstEl : buildPath ((dropColumns . dropRows) rows')
        where
          firstEl = head (head rows')
          dropRows = drop slopeY
          dropColumns = map (drop slopeX)

  treesInPath :: String -> Int
  treesInPath = length . filter (== '#')

  run = do
    input <- readLinesfromFile "./data/3.txt"
    print $ (treesInPath . getPath input) (3, 1)
    print $ product $ map (treesInPath . getPath input) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]