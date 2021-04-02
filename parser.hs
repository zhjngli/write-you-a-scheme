import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.List
import Numeric
import Control.Monad.Identity
import Control.Monad.Except
import Text.Parsec.Prim (ParsecT)
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
eval (List (Atom f : args)) = mapM eval args >>= apply f
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- apply :: String -> [LispVal] -> LispVal
-- apply f args = maybe (Bool False) ($ args) $ lookup f primitives

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
              ("symbol?", unaryOp testSymbol),
              ("string?", unaryOp testSymbol),
              ("number?", unaryOp testNumber),
              ("list?", unaryOp testList),
              ("char?", unaryOp testChar),
              ("bool?", unaryOp testBool),
              ("symbol->string", unaryOp symbolToString),
              ("string->symbol", unaryOp stringToSymbol)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp f vs = throwError $ NumArgs 1 vs

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" (String n)
                              else return $ fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
     (expr:_) <- getArgs
     let evaled = fmap show $ readExpr expr >>= eval
     putStrLn $ extractValue $ trapError evaled
