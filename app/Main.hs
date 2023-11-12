{-# LANGUAGE LambdaCase #-}
module Main where


import Data.Functor.Identity (Identity)
import Control.Monad (guard)

import qualified Control.Monad.State as M1

import Text.Parsec hiding (uncons)
import Data.List (singleton)

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



lispParse :: ParsecT String u Identity SExpression
lispParse = do
  char '('
  first <- lispParseSExpr
  skipMany space
  exprs <- sepBy (try lispParseSExpr <|> lispParse) spaces
  char ')'
  return $ List $ first : exprs




data LispResult = ResultNumber Integer
                  | ResultLambda { runLambda :: [LispResult] -> LispResult}

instance Show LispResult where
  show (ResultNumber x) = show x
  show (ResultLambda _) = "lambda"




newtype LispSymbolTable = LispSymbolTable {
  lispSymbol :: [(String,  LispResult)]
}

lispNumBinOp :: (Integer -> Integer -> Integer) -> [LispResult] -> LispResult
lispNumBinOp f ((ResultNumber l): ((ResultNumber r):_)) = ResultNumber $ f l r
lispNumBinOp _ _ = error "error"




lispGLobalEnv :: LispSymbolTable
lispGLobalEnv = LispSymbolTable [
  ("+", ResultLambda $ lispNumBinOp (+)),
  ("-", ResultLambda $ lispNumBinOp (+)),
  ("*", ResultLambda $ lispNumBinOp (+)),
  ("begin", ResultLambda last)
  ]

symTblAddVar :: (String, LispResult) ->  M1.State LispSymbolTable ()
symTblAddVar (s, v) = do
  sym <- M1.get
  M1.put $ LispSymbolTable ((s, v):lispSymbol sym)



lispEval :: SExpression -> M1.State LispSymbolTable LispResult
-- match when x is lambda
lispEval (List (Atom x :xs)) = do
  sym <- M1.get
  case x of
    --Atom "define" -> 
    "define" ->
      let (Atom s) = head xs in
        let exp = last xs in do
          v <- lispEval exp
          symTblAddVar ( s, v)
          return v
    "lambda" ->
      let (List ss) = head xs in
        let exp = last xs in do
          return $ ResultLambda $ \args -> let tbl = lispSymbol sym in
            let myx = zipWith (\ sss aa -> let (Atom ssss) = sss in (ssss, aa)) ss args in
            M1.evalState (lispEval exp) (LispSymbolTable (myx ++ tbl))
    --Atom "if" ->
    _ ->
     case lookup x (lispSymbol sym) of
      Just (ResultLambda f) -> do
        er <-  mapM lispEval xs
        return (f er)
      Just (ResultNumber n) -> return (ResultNumber n)
      Nothing -> error "no"
lispEval (Number x) = pure (ResultNumber x)
lispEval (Atom x) = do
  sym <- M1.get
  case lookup x (lispSymbol sym) of
        Just (ResultLambda f) -> do
          return $ f []
        Just (ResultNumber n) -> return (ResultNumber n)
        Nothing -> error "no such"
lispEval _ = error "grammar error"



lispRun :: String -> LispResult
lispRun x = M1.evalState (lispEval (case runParser lispParse "" "" x of
  Left err -> error $ show err
  Right val -> val)) lispGLobalEnv





main :: IO ()
main = do
  print $ lispRun "(define myfun (+ 1 2))"
  print $ lispRun "(begin (define myfun (+ 1 (* 2 24432))) myfun)"
  print $ lispRun "(begin (define myfun (lambda (x) (+ x 132))) (myfun 1))"