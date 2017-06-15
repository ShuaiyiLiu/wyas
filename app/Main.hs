{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Text.RawString.QQ

data LispVal =
  Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"abc")
  _ <- char '"'
  eof
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

-- Exercises 1.1
parseNumberE11 :: Parser LispVal
parseNumberE11 = do
  digits <- many1 digit
  return $ (Number . read) digits

-- Exercises 1.2
parseNumberE12 :: Parser LispVal
parseNumberE12 =
  many1 digit >>=
    \digits -> return $ (Number . read) digits

-- Exercise 2
parseStringE2 :: Parser LispVal
parseStringE2 = do
  _ <- char '"'
  x <- many (noneOf "\\\"" <|> (char '\\' >> char '"'))
  _ <- char '"'
  return $ String x

-- Exercise 3
parseStringE3 :: Parser LispVal
parseStringE3 = do
  _ <- char '"'
  let escapeParser = char '\\' >> oneOf "\n\r\t\\\""
  x <- many (noneOf "\\" <|> escapeParser)
  _ <- char '"'
  return $ String x

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  hd <- endBy parseExpr spaces
  tl <- char '.' >> spaces >> parseExpr
  return $ DottedList hd tl

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do _ <- char '('
         x <- try parseList <|> parseDottedList
         _ <- char ')'
         return x

main :: IO ()
main = do
  (expr:_) <- getArgs
  print expr
  putStrLn (readExpr expr)
