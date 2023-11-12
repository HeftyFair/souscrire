{-# LANGUAGE LambdaCase #-}
module Main where


import Data.Functor.Identity (Identity)
import Control.Monad (guard)

import qualified Control.Monad.State as M1

import Text.Parsec hiding (uncons)
import Data.List (singleton)


data LispAST = LispInt Integer
              | LispVar String
              | LispApp [LispAST]
              | LispDefine String LispAST
              | LispLambda [LispAST] LispAST
              deriving (Show)


data LispValue = LispInteger Integer
                | LispString String
                  deriving (Show)

data LispSymTbl = LispSymTbl {
  getLispSymTbl :: [(String, LispValue)],
  -- getBinOpFunc :: [(String, LispValue -> LispValue -> LispValue)],
  getBeginFunc :: [(String, [LispValue] -> LispValue)],
  -- lambdas :: [(String, ([String], LispAST))]
  lambdas :: [([LispAST], LispAST)]
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



lispIntValue :: ParsecT String u Identity Integer
lispIntValue = do
  x <- many1 digit
  return $ read x

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
    -- "lambda" -> let () = exprs in LispLambda 
    _ -> LispApp (LispVar sym:exprs)
      )




data SExpression = Atom String
                 | Number Integer
                 | List [SExpression]
                 -- | DotPair SExpression SExpression
                 deriving (Show)


lispParseStr :: ParsecT String u Identity SExpression
lispParseStr = do
  let p = letter <|> oneOf "+-*/"
  s <- many1 p
  return $ Atom s

lispParseNum :: ParsecT String u Identity SExpression
lispParseNum = do
  n <- many1 digit
  return $ Number $ read n

lispParseSExpr :: ParsecT String u Identity SExpression
lispParseSExpr = do
  try lispParseStr <|> lispParseNum


lispParse2 :: ParsecT String u Identity [SExpression]
lispParse2 = do
  char '('
  first <- lispParseSExpr
  skipMany space
  exprs <- sepBy (try lispParseSExpr <|> (List <$> lispParse2)) spaces
  char ')'
  return $ first : exprs



lispParse3 :: ParsecT String u Identity SExpression
lispParse3 = do
  char '('
  first <- lispParseSExpr
  skipMany space
  exprs <- sepBy (try lispParseSExpr <|> lispParse3) spaces
  char ')'
  return $ List $ first : exprs


newtype LispSymbolTable = LispSymbolTable {
  getFunc :: [(String, [SExpression] -> SExpression)]
}

lispNumBinOp :: (Integer -> Integer -> Integer) -> [SExpression] -> SExpression
lispNumBinOp f ((Number l): ((Number r):_)) = Number $ f l r
lispNumBinOp _ _ = error "error"

lispGLobalEnv :: LispSymbolTable
lispGLobalEnv = LispSymbolTable [
  ("+", lispNumBinOp (+)),
  ("-", lispNumBinOp (-)),
  ("*", lispNumBinOp (*)),
  ("begin", last)
  ]

symTblAddLambda :: (String, [SExpression] -> SExpression) ->  M1.State LispSymbolTable ()
symTblAddLambda s = do
  sym <- M1.get
  M1.put $ LispSymbolTable (s:getFunc sym)


symTblAddVar :: (String, SExpression) ->  M1.State LispSymbolTable ()
symTblAddVar (s, v) = do
  sym <- M1.get
  M1.put $ LispSymbolTable ((s, const v):getFunc sym)


lispEval2 :: [SExpression] -> M1.State LispSymbolTable SExpression
-- match when x is lambda
lispEval2 (Atom x: xs) = do
  sym <- M1.get
  case x of
    --Atom "define" -> 
    "define" ->
      let (Atom s) = head xs in
        let exp = tail xs in do
          v <- lispEval2 exp
          symTblAddVar ( s, v)
          return v
    "lambda" -> return $ Atom "dwdw"
    --Atom "if" ->
    _ ->
     case lookup x (getFunc sym) of
      Just f -> do
        er <-  mapM lispEval2 [xs]
        return (f er)
      Nothing -> error "no"
lispEval2 (List x: xs) = lispEval2 x
lispEval2 _ = error "13123"



data LispResult = ResultNumber Integer
                  | ResultLambda { runLambda :: [LispResult] -> LispResult}

instance Show LispResult where
  show (ResultNumber x) = show x
  show (ResultLambda _) = "lambda"


lispEval3 :: SExpression -> M1.State LispSymbolTable SExpression
-- match when x is lambda
lispEval3 (List (Atom x :xs)) = do
  sym <- M1.get
  case x of
    --Atom "define" -> 
    "define" ->
      let (Atom s) = head xs in
        let exp = last xs in do
          v <- lispEval3 exp
          symTblAddVar ( s, v)
          return v
    "lambda" ->
      let (List xs) = head xs in
        let exp = last xs in do
          return $ List xs
    --Atom "if" ->
    _ ->
     case lookup x (getFunc sym) of
      Just f -> do
        er <-  mapM lispEval3 xs
        return (f er)
      Nothing -> error "no"
lispEval3 (Number x) = pure (Number x)
lispEval3 (Atom x) = do
  sym <- M1.get
  case lookup x (getFunc sym) of
        Just f -> do
          return $ f []
        Nothing -> error "no such"
lispEval3 _ = error "grammar error"




newtype LispSymbolTable2 = LispSymbolTable2 {
  getFunc2 :: [(String,  LispResult)]
}

lispNumBinOp2 :: (Integer -> Integer -> Integer) -> [LispResult] -> LispResult
lispNumBinOp2 f ((ResultNumber l): ((ResultNumber r):_)) = ResultNumber $ f l r
lispNumBinOp2 _ _ = error "error"




lispGLobalEnv2 :: LispSymbolTable2
lispGLobalEnv2 = LispSymbolTable2 [
  ("+", ResultLambda $ lispNumBinOp2 (+)),
  ("-", ResultLambda $ lispNumBinOp2 (+)),
  ("*", ResultLambda $ lispNumBinOp2 (+)),
  ("begin", ResultLambda last)
  ]

symTblAddVar2 :: (String, LispResult) ->  M1.State LispSymbolTable2 ()
symTblAddVar2 (s, v) = do
  sym <- M1.get
  M1.put $ LispSymbolTable2 ((s, v):getFunc2 sym)

lookupFirst :: Eq a => a -> [(a, b, c)] -> Maybe (b, c)
lookupFirst _ [] = Nothing
lookupFirst key ((x, y, z):xs)
    | key == x = Just (y, z)
    | otherwise = lookupFirst key xs






lispEval4 :: SExpression -> M1.State LispSymbolTable2 LispResult
-- match when x is lambda
lispEval4 (List (Atom x :xs)) = do
  sym <- M1.get
  case x of
    --Atom "define" -> 
    "define" ->
      let (Atom s) = head xs in
        let exp = last xs in do
          v <- lispEval4 exp
          symTblAddVar2 ( s, v)
          return v
    "lambda" ->
      let (List ss) = head xs in
        let exp = last xs in do
          return $ ResultLambda $ \args -> let tbl = getFunc2 sym in
            let myx = zipWith (\ sss aa -> let (Atom ssss) = sss in (ssss, aa)) ss args in
            M1.evalState (lispEval4 exp) (LispSymbolTable2 (myx ++ tbl))
    --Atom "if" ->
    _ ->
     case lookup x (getFunc2 sym) of
      Just (ResultLambda f) -> do
        er <-  mapM lispEval4 xs
        return (f er)
      Just (ResultNumber n) -> return (ResultNumber n)
      Nothing -> error "no"
lispEval4 (Number x) = pure (ResultNumber x)
lispEval4 (Atom x) = do
  sym <- M1.get
  case lookup x (getFunc2 sym) of
        Just (ResultLambda f) -> do
          return $ f []
        Just (ResultNumber n) -> return (ResultNumber n)
        Nothing -> error "no such"
lispEval4 _ = error "grammar error"



lispRun2 :: String -> SExpression
lispRun2 x = M1.evalState (lispEval2 (case runParser lispParse2 "" "" x of
  Left err -> error $ show err
  Right val -> val)) lispGLobalEnv

lispRun3 :: String -> SExpression
lispRun3 x = M1.evalState (lispEval3 (case runParser lispParse3 "" "" x of
  Left err -> error $ show err
  Right val -> val)) lispGLobalEnv



lispRun4 :: String -> LispResult
lispRun4 x = M1.evalState (lispEval4 (case runParser lispParse3 "" "" x of
  Left err -> error $ show err
  Right val -> val)) lispGLobalEnv2


lispRun :: String -> LispValue
lispRun x = M1.evalState (lispEval (case runParser lispParse "" "" x of
  Left err -> error $ show err
  Right val -> val)) lispEnv


main :: IO ()
main = do
  print $ runParser lispParse3  "" "" "(begin (define myfun (+ 1 (* 2 24432))) myfun)"
  print $ lispRun4 "(define myfun (+ 1 2))"
  print $ lispRun4 "(begin (define myfun (+ 1 (* 2 24432))) myfun)"
  print $ lispRun4 "(begin (define myfun (lambda (x) (+ x 132))) (myfun 1))"
  print $ runParser lispParse3  "" "" "(begin (define myfun (lambda (x) (+ x 132))) (myfun 1))"
  --print $ lispRun3 "(begin (define myfun (lambda (x) (+ x 132))) (myfun 1))"
  --print $  M1.evalState (mytest "1") 2