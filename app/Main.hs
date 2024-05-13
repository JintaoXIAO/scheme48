{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment (getArgs)
import Control.Monad (liftM)
import Data.Function ((&))
import Control.Exception (Exception)
import Control.Monad.Error.Class (MonadError(catchError, throwError))

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr inp = case parse parseExpr "(lisp)" inp of
  Left err -> throwError $ Parser err
  Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Bool _) = return val
eval val@(Number _) = return val
eval (List [Atom "quota", val]) = return val
eval (List [Atom "if", cond, conseq, alt]) = do
  rst <- eval cond
  case rst of
    Bool False -> eval alt
    _ -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)
             ,("=", numBoolBinop (==))
             ,("<", numBoolBinop (<))
             ,(">", numBoolBinop (>))
             ,("/=", numBoolBinop (/=))
             ,(">=", numBoolBinop (>=))
             ,("<=", numBoolBinop (<=))
             ,("&&", boolBoolBinop (&&))
             ,("||", boolBoolBinop (||))
             ,("string=?", strBoolBinop (==))
             ,("string?", strBoolBinop (>))
             ,("string<=?", strBoolBinop (<=))
             ,("string>=?", strBoolBinop (>=))
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("eq?", eqv)
             ,("eqv?", eqv)
             ,("equal?", equal)
             ]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return . Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return . show $ n
unpackStr (Bool b) = return . show $ b
unpackStr others = throwError $ TypeMismatch "string" others

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool others = throwError $ TypeMismatch "boolean" others

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params =  mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum str@(String s) = let parsed = reads s in
                          if null parsed
                          then throwError $ TypeMismatch "number" str
                          else return . fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgs = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return . List $ xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return (DottedList xs x)
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgs = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs x'] = return $ DottedList ([x] ++ xs) x'
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgs = throwError $ NumArgs 2 badArgs

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool a1), (Bool a2)] = return $ Bool $ a1 == a2
eqv [(Number a1), (Number a2)] = return $ Bool $ a1 == a2
eqv [(String a1), (String a2)] = return $ Bool $ a1 == a2
eqv [(Atom a1), (Atom a2)] = return $ Bool $ a1 == a2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List a1, List a2] = return $ Bool $
  (length a1 == length a2) && (and $ map eqvPair $ zip a1 a2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                              Right (Bool val) -> val
                              _ -> False
eqv [_, _] = return $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

data Unpacker =  forall a. Eq a
              => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum
                                                   ,AnyUnpacker unpackStr
                                                   ,AnyUnpacker unpackBool
                                                   ]
  eqvEquals <- eqv [arg1, arg2]
  case eqvEquals of
    (Bool x) -> return $ Bool $ (primitiveEquals || x)
    _ -> return $ Bool $ (primitiveEquals || False)
equal badArgs = throwError $ NumArgs 2 badArgs


-- exceptions

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar msg name) = msg ++ ": " ++ name
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (NumArgs expected found) =  "Expected " ++ show expected
                                   ++ " args: found values "
                                   ++ unwordsList found
showError (TypeMismatch expected found) =  "Invalid type: expected "
                                        ++ expected
                                        ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default str) = str

instance Show LispError where
  show = showError

instance Exception LispError

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = undefined

