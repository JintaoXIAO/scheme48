module Main where

import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)

readExpr :: String -> String
readExpr inp = case parse (spaces >> symbol) "(lisp)" inp of
  Left err -> "No match" ++ show err
  Right _ -> "Found value"

symbol :: Parser Char
symbol = oneOf " !$%&|*+-/:<=? >@^_~#"

spaces :: Parser ()
spaces = skipMany1 space