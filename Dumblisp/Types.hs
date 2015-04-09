module Dumblisp.Types (LispVal(Atom,List,DottedList,Number,FNum,String,Bool),
                       showVal,
                       Env,
                       LispError(NumArgs,TypeMismatch,Parser,BadSpecialForm,NotFunction,UnboundVar,Default),
                       ThrowsError,
                       trapError,
                       extractValue,
                       IOThrowsError,
                       liftThrows,
                       runIOThrows) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.IORef

instance Show LispVal where show = showVal
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | FNum Float
             | String String
             | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (FNum float) = show float
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

type Env = IORef [(String, IORef LispVal)]

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError
instance Error LispError where
  noMsg  = Default "An error has occured"
  strMsg = Default

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type expected " ++ expected ++ ", found" ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = message

type ThrowsError = Either LispError

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
