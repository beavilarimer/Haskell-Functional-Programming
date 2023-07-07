----------------------------------------------------------------------
-- S-expression parser.
----------------------------------------------------------------------

-- We call this module `Main` for testing purposes.
-- In a real-world scenario, it would be called `Sexpr`.
module Main where

import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String


----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA   Bool
  | IntA    Integer
  | FloatA  Double
  | IdA     String
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  eThere <- option "" (try (string "e") <|> try (string "E"))
  eSign <- option "" (try (string "-") <|> try (string "+"))
  eValue <- option "" (many1 digit)
  return (read (sign ++ digits ++ "." ++ f ++ eThere ++ eSign ++ eValue) :: Double)
  <?> "floating-point number"

parseString :: Parser String
parseString = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return s
  <?> "string"

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> (parseString >>= return . StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList =
  parseListHelper '(' ')'
  <|> parseListHelper '{' '}'
  <|> parseListHelper '[' ']' 

parseListHelper :: Char -> Char -> Parser [Sexpr]
parseListHelper b e = do
  char b
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char e
  return ss
  <?> "list of S-expressions"

{- We don't need the try statement because once the parser matches one of the
opening elements either (, [, or { it is not necessary to ever backtrack.
There is no common first element among these three that requires us to go back
if we detect a mistake, as it occured in the case of true/false (#t, #f) that
was discussed in lecture-}

-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = char '\'' >> parseSexpr
  <?> "quoted S-expression"

-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> (parseQuote >>= \q -> return (ListS [AtomS (IdA "quote"), q]))
  <?> "S-expression"

-- Parse a series of Sexprs from a string
-- representing the entire contents of a file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ "AtomS[" ++ show a ++ "]"
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

usage :: String -> IO ()
usage s = do
  hPutStrLn stderr $ "usage: " ++ s ++ " filename"

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [filename] -> runPpSexpr filename >> exitSuccess
    _ -> usage progName >> exitFailure
