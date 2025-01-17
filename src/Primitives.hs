{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Primitives
    (
      Env,
      IOThrowsError,
      LispError(..),
      LispVal(..),
      ThrowsError,
      showVal,
      nullEnv,
      getVar,
      setVar,
      defineVar,
      bindVars,
      liftThrows,
      runIOThrows
    ) where

import Control.Monad.Except
    ( liftM, runExceptT, MonadIO(liftIO), MonadError(..), ExceptT )
import Data.Array ( Array )
import Data.Complex ( Complex )
import Data.IORef ( newIORef, readIORef, writeIORef, IORef )
import System.IO
    ( Handle,
      stdout,
      hClose,
      openFile,
      stdin,
      hGetLine,
      hPrint,
      IOMode(WriteMode, ReadMode) )

import Text.ParserCombinators.Parsec ( ParseError )


data LispVal = Atom String
             | Vector (Array Int LispVal)
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | ComplexNumber (Complex Double)
             | Float Double
             | Rational Integer Integer
             | Number Integer
             | Character Char
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

type ThrowsError = Either LispError

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Character c) = "'" ++ show c ++ "'"
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Float f) = show f
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _)   = "<IO port>"

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- ENVIRONMENT

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

nullEnv :: IO Env
nullEnv = newIORef []

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . flip writeIORef value)
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
