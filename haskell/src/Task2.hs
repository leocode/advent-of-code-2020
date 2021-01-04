module Task2 where

  import Helpers (readLinesfromFile, countInString, toInt)

  import Text.Parsec
    ( anyChar,
      char,
      digit,
      letter,
      spaces,
      many1,
      many,
      parse,
      ParseError,
      Parsec )
  import Control.Monad.Identity (Identity)
  import Control.Exception

  type ParseResult = (String, String, Char, String)

  lineParser :: Parsec String () ParseResult
  lineParser = do
    min <- many1 digit
    char '-'
    max <- many1 digit
    spaces
    ltr <- letter
    char ':'
    spaces
    password <- many anyChar
    return (min, max, ltr, password)

  parseLine :: String -> Either ParseError ParseResult
  parseLine = parse lineParser ""

  verifyResult :: Either ParseError ParseResult -> Bool
  verifyResult (Left _) = False
  verifyResult (Right (min, max, char, password)) = 
    let count = countInString char password
    in count >= toInt min && count <= toInt max
      
  main = do
    input <- readLinesfromFile "./data/2.txt"
    print $ (length . (filter verifyResult . map parseLine)) input