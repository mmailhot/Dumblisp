module Dumblisp.Eval (eval) where

import Dumblisp.Types
import Control.Monad.Error

primitives :: [(String, [LispVal]-> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", checkType "symbol"),
              ("string?", checkType "string"),
              ("number?", checkType "num")]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []         = throwError $ NumArgs 2 []
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params     = mapM unpackNum params >>= return . Number . foldl1 op
 
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum 

checkType :: String -> [LispVal] -> ThrowsError LispVal
checkType "symbol" [(Atom _)] = return $ Bool True
checkType "string" [(String _)] = return $ Bool True
checkType "num" [(Number _)] = return $ Bool True
checkType _ _ = return $ Bool False

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
