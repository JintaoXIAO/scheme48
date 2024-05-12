module Main where

import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment (getArgs)
import Control.Monad (liftM)
import Data.Function ((&))

main :: IO ()
main = do
  args <- getArgs
  putStrLn . show . eval . readExpr $ (args !! 0)

readExpr :: String -> LispVal
readExpr inp = case parse parseExpr "(lisp)" inp of
  Left err -> String $ "No match" ++ show err
  Right val -> val

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
  return $ List [Atom "quota", x]

-- evaluate

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ "." ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Bool _) = val
eval val@(Number _) = val
eval (List [Atom "quota", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval _ = undefined

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = reads s in
                          if null parsed
                          then 0
                          else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0