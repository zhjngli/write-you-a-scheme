import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.List
import Numeric
import Control.Monad.Identity
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

symbol :: Parser Char
symbol = oneOf "!$%&|*+_/:<=>?@^_~"

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

printLispVal :: LispVal -> String
printLispVal val = case val of
    Atom a -> "Atom: " ++ show a
    List vs -> "List: [" ++ intercalate ", " (fmap printLispVal vs) ++ "]"
    DottedList vs v -> "Dotted: " ++ printLispVal (List vs) ++ " :: " ++ printLispVal v
    Number n -> "Number: " ++ show n
    Float f -> "Float: " ++ show f
    String s -> "String: " ++ s
    Character c -> "Character: " ++ show c
    Bool b -> "Bool: " ++ show b

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ printLispVal val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn expr
    putStrLn (readExpr expr)