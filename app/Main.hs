module Main where

import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment (getArgs)
import Control.Monad (liftM)
import Data.Function ((&))

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)

readExpr :: String -> String
readExpr inp = case parse parseExpr "(lisp)" inp of
  Left err -> "No match" ++ show err
  Right val -> "Found value: " ++ show val

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  return $ case atom of
              "#t" -> Bool True
              "#f" -> Bool False
              _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber =
  liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr =   parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do _ <- char '('
                 x <- (try parseList) <|> parseDottedList
                 _ <- char ')'
                 return x

parseList :: Parser LispVal
parseList = parseExpr `sepBy` spaces
          & liftM List

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- parseExpr `endBy` spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
