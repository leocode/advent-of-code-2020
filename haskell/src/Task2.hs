module Task2 where

  import Helpers (readLinesfromFile, countInString, toInt, at)

  import qualified Text.Parsec as Parsec
  import Control.Monad.Identity (Identity)
  import Control.Exception

  type ParseResult = (String, String, Char, String)
  type Input = (Int, Int, Char, String)

  lineParser :: Parsec.Parsec String () ParseResult
  lineParser = do
    firstNumber <- Parsec.many1 Parsec.digit
    Parsec.char '-'
    secondNumber <- Parsec.many1 Parsec.digit
    Parsec.spaces
    letter <- Parsec.letter
    Parsec.char ':'
    Parsec.spaces
    password <- Parsec.many Parsec.anyChar
    return (firstNumber, secondNumber, letter, password)

  parseLine :: String -> Either Parsec.ParseError ParseResult
  parseLine = Parsec.parse lineParser ""

  oldPolicy :: Input -> Bool
  oldPolicy (min, max, char, password) =
    let count = countInString char password
    in count >= min && count <= max

  newPolicy :: Input -> Bool
  newPolicy (pos1, pos2, expectedChar, password) =
    newPolicyChecker (Just expectedChar) char1 char2
    where
      char1 = at (pos1 - 1) password
      char2 = at (pos2 - 1) password
      newPolicyChecker :: Eq a => a -> a -> a -> Bool
      newPolicyChecker expectedChar x y = (expectedChar == x || expectedChar == y) && x /= y

  verifyResult :: (Input -> Bool) -> Either Parsec.ParseError ParseResult -> Bool
  verifyResult _ (Left _) = False
  verifyResult policy (Right (min, max, char, password)) = policy (toInt min, toInt max, char, password)
      
  main = do
    input <- readLinesfromFile "./data/2.txt"
    print $ (length . (filter (verifyResult oldPolicy) . map parseLine)) input
    print $ (length . (filter (verifyResult newPolicy) . map parseLine)) input
