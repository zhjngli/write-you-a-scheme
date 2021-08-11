{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.List
import Numeric
import Control.Monad.Identity
import Control.Monad.Except
import Text.Parsec.Prim (ParsecT,Consumed (Consumed))
import Data.Char (digitToInt)
import Data.Complex
import Data.Array
import System.IO
import Data.IORef

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

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = do
    char '\\'
    oneOf "\"\\nrt"

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"\\" <|> escapeChar)
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    try $ char '#'
    b <- letter
    return $ case b of
        't' -> Bool True
        'f' -> Bool False

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    c <- string "space" <|> string "newline" <|>
        do {
            c <- anyChar;
            notFollowedBy alphaNum;
            return [c]
        }
    return $ case c of
        "space" -> Character ' '
        "newline" -> Character '\n'
        _ -> Character $ head c

toDouble :: LispVal -> Double
toDouble (Float d) = d
toDouble (Number n) = fromIntegral n

parseComplexNumber :: Parser LispVal
parseComplexNumber = do
    r <- parseFloat <|> parseNumber
    char '+'
    i <- parseFloat <|> parseNumber
    char 'i'
    return $ ComplexNumber (toDouble r :+ toDouble i)

parseFloat :: Parser LispVal
parseFloat = do
    i <- many1 digit
    char '.'
    d <- many1 digit
    return $ Float . fst . head $ readFloat (i ++ "." ++ d)

parseRational :: Parser LispVal
parseRational = do
    numerator <- many1 digit
    char '/'
    denominator <- many1 digit
    return $ Rational (fst . head $ readDec numerator) (fst . head $ readDec denominator)

-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = many1 digit >>= \ds -> return $ (Number . read) ds
parseNum :: ParsecT String () Identity Char -> (String -> Integer) -> Parser LispVal
parseNum dig readf = do
    ds <- many1 dig
    return $ (Number . readf) ds

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    parseNum (oneOf "01") (fst . foldr (\b (s, p) -> (s + (toInteger . digitToInt) b * 2^p, p + 1)) (0, 0))

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    parseNum octDigit (fst . head . readOct)

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    parseNum hexDigit (fst . head . readHex)

parseDec :: Parser LispVal
parseDec = do
    try $ string "#d"
    parseNum digit (fst . head . readDec)

parseNumber :: Parser LispVal
parseNumber = parseNum digit (fst . head . readDec) <|> parseDec <|> parseHex <|> parseOct <|> parseBin

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
    string ",@"
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

parseSchemeList :: Parser LispVal
parseSchemeList = do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

parseVector :: Parser LispVal
parseVector = do
    string "#("
    vs <- sepBy parseExpr spaces
    char ')'
    return $ Vector (listArray (0, length vs - 1) vs)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplexNumber
        <|> try parseFloat
        <|> try parseRational
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnquoted
        <|> parseUnquoteSplicing
        <|> parseSchemeList


-- EVALUATOR

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc :: Monad m => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing
makeVarArgs :: Monad m => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", cond, t, f]) = do
    p <- eval env cond
    case p of
        Bool True  -> eval env t
        Bool False -> eval env f
        _          -> throwError $ TypeMismatch "boolean" p
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
eval env (List (Atom "cond" : args)) = cond env args
eval env (List (Atom "case" : args)) = caseL env args
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env [List [Atom "else", clause]] = eval env clause
cond env ((List [test, val]) : clauses) = do
    t <- eval env test
    case t of
        Bool True -> return val
        Bool False -> cond env clauses
cond env ((List a) : _) = throwError $ NumArgs 2 a
cond env (a : _) = throwError $ NumArgs 2 [a]
cond env _ = throwError $ Default "no alternative in cond"

caseL :: Env -> [LispVal] -> IOThrowsError LispVal
caseL env [key, List (Atom "else" : exprs)] = eval env $ last exprs
caseL env (key : List ((List datum) : exprs) : clauses) = do
    k <- eval env key
    matches <- mapM (\x -> liftThrows $ eqv [k, x]) datum
    let eq = foldl' (\b m -> case b of
                Bool False -> m
                Bool True -> Bool True)
            (Bool False) matches
    case eq of
        Bool True -> eval env $ last exprs
        Bool False -> caseL env (key : clauses)
caseL env ((List a) : _) = throwError $ NumArgs 2 a
caseL env (a : _) = throwError $ NumArgs 2 [a]
caseL env _ = throwError $ Default "no alternative in case"

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
apply (IOFunc func) args = func args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("make-string", makeString),
              ("string-length", stringLen),
              ("string-ref", stringRef),
              ("substring", substring),
              ("string-append", stringAppend),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", unaryOp testSymbol),
              ("string?", unaryOp testSymbol),
              ("number?", unaryOp testNumber),
              ("list?", unaryOp testList),
              ("char?", unaryOp testChar),
              ("bool?", unaryOp testBool),
              ("symbol->string", unaryOp symbolToString),
              ("string->symbol", unaryOp stringToSymbol),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp f vs = throwError $ NumArgs 1 vs

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [a, b] = do
    left <- unpacker a
    right <- unpacker b
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args
-- boolBinop unpacker op args = if length args /= 2
--                              then throwError $ NumArgs 2 args
--                              else do left <- unpacker $ args !! 0
--                                      right <- unpacker $ args !! 1
--                                      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" (String n)
                              else return $ fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    catchError
    (do
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return $ unpacked1 == unpacked2)
    (const $ return False)

makeString, stringLen, stringRef, substring, stringAppend, stringToList, listToString :: [LispVal] -> ThrowsError LispVal
makeString [Number n, Character c] =
    if n < 0 then throwError $ Default "Cannot make string with negative characters"
    else return $ String $ mkStr (fromInteger n) c ""
    where mkStr 0 c l = l
          mkStr n c l = mkStr (n - 1) c (c:l)
makeString [Number n, v] = throwError $ TypeMismatch "character" v
makeString [v, Character c] = throwError $ TypeMismatch "number" v
makeString l = throwError $ NumArgs 2 l

stringLen [String s] = return $ Number . toInteger $ length s
stringLen [v] = throwError $ TypeMismatch "string" v
stringLen l = throwError $ NumArgs 1 l

stringRef [String s, Number n] = let i = fromInteger n in
    if i >= length s then throwError $ Default "Index out of bounds"
    else return $ Character (last (take (i + 1) s))
stringRef [String s, v] = throwError $ TypeMismatch "number" v
stringRef [v, Number n] = throwError $ TypeMismatch "string" v
stringRef l = throwError $ NumArgs 2 l

substring [String s, Number start, Number end] =
    let len = length s in
    let st = fromInteger start in
    let e = fromInteger end in
    if st < 0 || st > e || e > len then throwError $ Default "Cannot make substring with given indexes"
    else return $ String $ take (e - st) $ drop st s
substring [String s, Number start, v] = throwError $ TypeMismatch "number" v
substring [String s, v, _] = throwError $ TypeMismatch "number" v
substring [v, _, _] = throwError $ TypeMismatch "string" v
substring l = throwError $ NumArgs 3 l

stringAppend (String s:l) = case stringAppend l of
    Right (String ss) -> return $ String (s ++ ss)
    Left err -> throwError err
stringAppend (v:l) = throwError $ TypeMismatch "string" v
stringAppend [] = return $ String ""

stringToList [String s] = return $ List cs
    where cs = foldl' (\a c -> Character c : a) [] s
stringToList [v] = throwError $ TypeMismatch "string" v
stringToList v = throwError $ NumArgs 1 v

listToString (Character c:l) = case listToString l of
    Right (String s) -> return $ String (c:s)
    Left err -> throwError err
listToString (v:l) = throwError $ TypeMismatch "character" v
listToString [] = return $ List []

testSymbol, testString, testNumber, testList, testBool, testChar :: LispVal -> LispVal
testSymbol (Atom _) = Bool True
testSymbol _ = Bool False

testString (String _) = Bool True
testString _ = Bool False

testNumber (Number _) = Bool True
testNumber _ = Bool False

testList (List _) = Bool True
testList (DottedList _ _) = Bool False
testList _ = Bool False

testChar (Character _) = Bool True
testChar _ = Bool False

testBool (Bool _) = Bool True
testBool _ = Bool False

symbolToString, stringToSymbol :: LispVal -> LispVal
symbolToString (Atom a) = String a
symbolToString _ = String ""
stringToSymbol (String s) = Atom s
stringToSymbol _ = Atom ""

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x -- NOT List [x] (lookup cdr definition)
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

equalList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> [LispVal] -> ThrowsError LispVal
equalList equalFunc l1 l2 = return $ Bool $ (length l1 == length l2) && all equalPair (zip l1 l2)
    where equalPair (x1, x2) = case equalFunc [x1, x2] of
                                Left err -> False
                                Right (Bool b) -> b

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = equalList eqv arg1 arg2
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ NumArgs 2 badArgList


equal :: [LispVal] -> ThrowsError LispVal
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [List l1, List l2] = equalList equal l1 l2
equal [arg1, arg2] = do
      primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)


-- VARIABLES AND ASSIGNMENT

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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
     where makeFunc constructor (var, func) = (var, constructor func)


-- IO PRIMITIVES

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename



-- REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args
