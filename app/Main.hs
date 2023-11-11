module Main where


import Data.Functor.Identity (Identity)
import Control.Monad (guard)

import qualified Control.Monad.State as M1

import Text.Parsec hiding (uncons)

pLambda :: ParsecT String u Identity String
pLambda = string "lambda" <|> string "Lambda"






myparse :: ParsecT String Integer Identity String
myparse = do
  myv <- string "lambda"
  mystate <- getState
  putState 1
  return myv


newtype MySymTbl = MySymTbl { getSymTbl :: [(String, Integer)] } deriving (Show)



parseVar :: ParsecT String MySymTbl Identity Integer
parseVar = do
  myv <- many1 letter
  mystate <- getState
  let sym = getSymTbl mystate in
    case lookup myv sym of
      Just x -> return x
      Nothing -> fail "not found"



parseAbst :: ParsecT String String Identity ([Int] -> Int)
parseAbst = do
  _ <- string "λ"
  myv <- many1 letter
  _ <- string "."
  expr <- parseAbst
  return $ \x -> expr x


parseApp :: ParsecT String String Identity String
parseApp = do
  parseAbst
  spaces
  many1 digit

parseExpr :: ParsecT String String Identity String
parseExpr = do
  parseApp


myparse1 :: ParsecT String String Identity String
myparse1 = do
  myv <- string "lambda"
  mystate <- getState
  putState "testState"
  return myv
-- call pLambda
-- parse pLambda "lambda" "lambda"

parseDigit :: ParsecT String u Identity String
parseDigit = do
  x <- oneOf ['0'..'9']
  return [x]


lispBuiltinBinOps :: String -> Integer -> Integer -> Integer
lispBuiltinBinOps x y z = case x of
  "+" -> y + z
  "-" -> y - z
  "*" -> y * z
  "/" -> y `div` z
  _ -> error "unknown operator"

data LispValue = LispInteger Integer
                | LispString String
                  deriving (Show)



data LispAST = LispInt Integer
              | LispVar String
              | LispApp [LispAST]
              | LispDefine String LispAST
              deriving (Show)

data LispSymTbl = LispSymTbl {
  getLispSymTbl :: [(String, LispValue)],
  -- getBinOpFunc :: [(String, LispValue -> LispValue -> LispValue)],
  getBeginFunc :: [(String, [LispValue] -> LispValue)],
  lambdas :: [(String, ([String], LispAST))]
  }

binaryIntOp :: (Integer -> Integer -> Integer) -> LispValue -> LispValue -> LispValue
binaryIntOp op (LispInteger x) (LispInteger y) = LispInteger (op x y)
binaryIntOp _ _ _ = error "Arguments must be integers"



lispBinOps :: (Integer -> Integer -> Integer) -> [LispValue] -> LispValue
lispBinOps op x = if length x == 2
  then binaryIntOp op (head x) (last x)
  else error "Arguments must be two integers"

lispEnv :: LispSymTbl
lispEnv = LispSymTbl [] 
  [
    ("begin", last),
    ("+", lispBinOps (+)),
    ("-", lispBinOps (-)),
    ("*", lispBinOps (*)),
    ("/", lispBinOps div)
  ]
  []


instance Show LispSymTbl where
  show (LispSymTbl symTbl ops _) =
    "LispSymTbl { getLispSymTbl = " ++ show (map fst symTbl) ++
    ", getBinOpFunc = " ++ show (map fst ops) ++ " }"



data LispASt = LispAInt Integer
             | LispAVar String
             | LispAApp [LispASt]
             | LispALam String LispASt
             | LispABinOp String LispASt LispASt
             deriving (Show)

lispIntValue :: ParsecT String u Identity Integer
lispIntValue = do
  x <- many1 digit
  return $ read x


myfun :: String -> [a] -> a
myfun = const last


addSymbol :: (String, LispValue) ->  M1.State LispSymTbl ()
addSymbol s = do
  sym <- M1.get
  M1.put $ LispSymTbl (s:getLispSymTbl sym) (getBeginFunc sym) (lambdas sym)


lispEval ::  LispAST -> M1.State LispSymTbl LispValue
lispEval (LispInt x) = return (LispInteger x)
-- lispEval (LispApp [LispVar x, lo, ro]) = do
--   sym <- M1.get
--   case lookup x (getBinOpFunc sym) of
--     Just f -> do
--       lo' <- lispEval lo
--       ro' <- lispEval ro
--       return $ f lo' ro'
--     Nothing -> error "not found"
lispEval (LispApp ((LispVar x):xs)) = do
  sym <- M1.get
  case lookup x (getBeginFunc sym) of
    Just f -> do
      values <- mapM lispEval xs
      return $ f values
    Nothing -> error "not found"
  --values <- mapM lispEval xs
  --return (myfun x values)
lispEval (LispVar x) = do
  sym <- M1.get
  case lookup x (getLispSymTbl sym) of
    Just x -> return x
    Nothing -> error "not found"
lispEval (LispDefine astName ast) = do
  sym <- M1.get
  astValue <- lispEval ast
  addSymbol (astName, astValue)
  return astValue


testAST :: LispAST
testAST = LispApp [LispVar "+", LispInt 1, LispInt 2, LispInt 3]

testAST2 :: LispAST
testAST2 = LispApp [LispVar "+", LispInt 1, LispDefine "myfun" (LispInt 1356) , LispVar "myfun"]


testAST3 :: LispAST
testAST3 = LispApp [LispVar "+", LispInt 1, LispInt 3]

myresult = M1.evalState (lispEval testAST) lispEnv


myresult2  = M1.evalState (lispEval testAST3) lispEnv

parsePlus :: ParsecT String u Identity Integer
parsePlus = do
  char '+'
  skipMany1 space
  x <- lispIntValue
  skipMany1 space
  y <-  lispIntValue
  return $ x + y



lispParseInt :: ParsecT String u Identity LispAST
lispParseInt = do
  x <- many1 digit
  return $ LispInt $ read x

lispParseVar :: ParsecT String u Identity LispAST
lispParseVar = do
  x <- many1 (letter <|> oneOf "+-*/")
  return $ LispVar x

lispParseValue :: ParsecT String u Identity LispAST
lispParseValue = lispParseInt <|> lispParseVar

lispParse :: ParsecT String u Identity LispAST
lispParse = do
  char '('
  skipMany space
  sym <- many1 (letter <|> oneOf "+-*/")
  spaces
  exprs <- sepBy1 (lispParseValue <|> lispParse) spaces
  char ')'
  --eof
  return (case sym of
    "define" -> let ((LispVar e):es) = exprs in LispDefine e (head es)
    _ -> LispApp (LispVar sym:exprs)
      )


lispRun :: String -> LispValue
lispRun x = M1.evalState (lispEval (case runParser lispParse "" "" x of
  Left err -> error $ show err
  Right val -> val)) lispEnv


mytest :: String -> M1.State Integer Integer
mytest x = do
  s1 <- M1.get
  M1.put $ s1 + 1
  return (read x + s1)


main :: IO ()
main = do
  --putStrLn $  let x = runParser  "" "lambda" "λx.λy.x 1 2" in case x of
  -- putStrLn $ let x = runParser lispParse "" "lambda" "(define my (+ 1 2)" in case x of
  --   Left err -> show err
  --   Right val -> show val
  --print myresult
  --print myresult2
  --print $ let x = runParser lispParse "" "" "(define myfun (+ 1 2))" in case x of
  --  Left err -> show err
  --  Right val -> show val
  print $ lispRun "(define myfun (+ 1 2))"
  print $ lispRun "(begin (define myfun (+ 1 (* 2 24432))) myfun)"
  --print $  M1.evalState (mytest "1") 2