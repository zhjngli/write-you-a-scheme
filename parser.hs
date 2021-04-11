{-# LANGUAGE ExistentialQuantification #-}
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

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

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

-- printLispVal :: LispVal -> String
-- printLispVal val = case val of
--     Atom a -> "Atom: " ++ show a
--     List vs -> "List: [" ++ intercalate ", " (fmap printLispVal vs) ++ "]"
--     DottedList vs v -> "Dotted: " ++ printLispVal (List vs) ++ " :: " ++ printLispVal v
--     Number n -> "Number: " ++ show n
--     Float f -> "Float: " ++ show f
--     String s -> "String: " ++ s
--     Character c -> "Character: " ++ show c
--     Bool b -> "Bool: " ++ show b

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, t, f]) = do
    p <- eval cond
    case p of
        Bool True  -> eval t
        Bool False -> eval f
        _          -> throwError $ TypeMismatch "boolean" p
eval (List (Atom "cond" : args)) = cond args
eval (List (Atom "case" : args)) = caseL args
eval (List (Atom f : args)) = mapM eval args >>= apply f
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

clause, lastClause :: LispVal -> ThrowsError (Maybe [LispVal])
clause (List [test, Atom "=>", expr]) = clause $ List [test, expr]
clause (List (test:exprs)) = case eval test of
    Left err -> throwError err
    Right val -> case val of
        Bool True -> Right (Just exprs)
        Bool False -> Right Nothing
        _ -> throwError $ TypeMismatch "boolean" val
clause badClause = throwError $ BadSpecialForm "cond clause bad form" badClause

lastClause (List (Atom "else" : exprs)) = Right (Just exprs)
lastClause c = clause c

cond :: [LispVal] -> ThrowsError LispVal
cond clauses =
    let firstClausesResult = foldl' f (Right Nothing) $ init clauses
                             where f a c = case a of
                                            Left err -> throwError err
                                            Right (Just exprs) -> a
                                            Right Nothing -> clause c
    in case firstClausesResult of
        Left err -> throwError err
        Right (Just exprs) -> Right $ List exprs
        Right Nothing -> case lastClause $ last clauses of
                            Left err -> throwError err
                            Right (Just exprs) -> Right $ List exprs
                            Right Nothing -> throwError $ BadSpecialForm "no cond evaluated to true" $ List clauses

caseClause, lastCaseClause :: ThrowsError LispVal -> LispVal -> ThrowsError (Maybe [LispVal])
caseClause (Right key) (List (List datum : exprs)) =
    let keyInDatum = foldl' f False datum
                     where f a d = a || b
                            where b = case eqv [key, d] of
                                    Left err -> False
                                    Right (Bool b) -> b
    in if keyInDatum then Right $ Just exprs else Right Nothing
caseClause (Right key) badClause = throwError $ BadSpecialForm "case clause bad form" badClause

lastCaseClause (Right key) (List (Atom "else" : exprs)) = Right (Just exprs)
lastCaseClause (Right key) c = caseClause (Right key) c

caseL :: [LispVal] -> ThrowsError LispVal
caseL (key : exprs) =
    let kval = case eval key of
                Left err -> throwError err
                Right v -> Right v
    in
    let firstClausesResult = foldl' f (Right Nothing) $ init exprs
                             where f a c = case a of
                                            Left err -> throwError err
                                            Right (Just exprs) -> a
                                            Right Nothing -> caseClause kval c
    in case firstClausesResult of
        Left err -> throwError err
        Right (Just ex) -> Right $ List ex
        Right Nothing -> case lastCaseClause kval (last exprs) of
                            Left err -> throwError err
                            Right (Just exprs) -> Right $ List exprs
                            Right Nothing -> throwError $ BadSpecialForm "no case evaluated to true" $ List exprs

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "Unrecognized primitive function args" f)
                     ($ args)
                     (lookup f primitives)

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
     (expr:_) <- getArgs
     let evaled = fmap show $ readExpr expr >>= eval
     putStrLn $ extractValue $ trapError evaled
