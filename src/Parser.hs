{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           RIO                     hiding ( some
                                                , many
                                                , try
                                                , (<|>)
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Data.Map                      as M
import           Data.Void                      ( Void )
import           Prelude                        ( init
                                                , last
                                                )
import           Data.Char                      ( intToDigit
                                                , chr
                                                )

-- type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void String a

data LispExpr
  = List [LispExpr]
  | Variable String
  | EConstant LispDatum
  | Quote LispDatum
  | Lambda LispExpr LispExpr -- first can be a symbol, list of symbols or cons of symbols
  | If LispExpr LispExpr LispExpr
  | Application LispExpr
  | Set LispExpr LispExpr
  | Quasiquote LispDatum
  | Unquote LispDatum
  | Cons LispExpr -- tail : head
  deriving (Show, Eq)
-- append? vector?

data LispDatum
  = DConstant Constant
  | DSymbol String
  | DList [LispDatum]
  | DVector [LispDatum]
  deriving (Show, Eq)

data Constant
  = LBool Bool
  | LNum Number
  | LChar Char
  | LString String
  deriving (Show, Eq)


dBool = DConstant . LBool
dNum = DConstant . LNum
dChar = DConstant . LChar
dString = DConstant . LString

lBool = EConstant . dBool
lNum = EConstant . dNum
lChar = EConstant . dChar
lString = EConstant . dString


data Number
  = Integral Int
  | Floating Float
  deriving (Show, Eq)

-- symbolChars :: String
-- symbolChars = ".!#$%&|*+-/:<=>?@^_~"

digits :: String
digits = map intToDigit [0 .. 9]

letters :: String
letters = map chr ([65 .. 90] ++ [97 .. 122])

initialCharacters :: String
initialCharacters = "!$%&*/:<=>?~_^"

nonInitialCharacters :: String
nonInitialCharacters = ".+-"

initialChar :: Parser Char
initialChar = letterChar <|> oneOf initialCharacters

subsequentChar :: Parser Char
subsequentChar = initialChar <|> digitChar <|> oneOf nonInitialCharacters

abbrs :: Map String (String, Parser LispDatum, LispDatum -> LispExpr)
abbrs = M.fromList
  [ ("`", ("quasiquote", datum, Quasiquote))
  , ("'", ("quote", datum, Quote))
  , (",", ("unquote", datum, Unquote))
  -- ,@
  ]

abbreviations :: [String]
abbreviations = M.keys abbrs -- ["`", "'", ","]

-- abbreviationsT :: [Text]
-- abbreviationsT = map pack abbreviations

-- we are matching 2 characters tops
matchAbbreviation :: String -> Maybe String
matchAbbreviation abb@(x : _) | M.member abb abbrs = Just abb
                              | M.member [x] abbrs = Just [x]
                              | otherwise          = Nothing
matchAbbreviation [] = Nothing

-- TODO add comments - they start with a semicolon
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

word :: String -> Parser String
word = L.symbol spaceConsumer

stringP :: Parser String
stringP = char '\"' *> manyTill L.charLiteral (char '\"')

parens :: Parser a -> Parser a
parens = between (word "(") (word ")")

expr :: Parser LispExpr
expr = do
  pairCh <- peekPair
  lexeme $ switch1 pairCh
 where
  switch1 :: String -> Parser LispExpr
  switch1 pair = case matchAbbreviation pair of
    Just abb -> expander abb
    Nothing  -> switch2 pair
  switch2 :: String -> Parser LispExpr
  switch2 [] = empty
  switch2 (ch1 : _) | ch1 == '('                      = list
                    | ch1 == '"' = lString <$> literalString
                    | ch1 `elem` nonInitialCharacters = symbolOrFloat
                    | otherwise                       = symbol

peekPair :: Parser [Char]
peekPair = lookAhead $ do
  c1 <- asciiChar
  c2 <- asciiChar
  return [c1, c2]

peekOne :: Parser Char
peekOne = lookAhead asciiChar

symbol :: Parser LispExpr
symbol = do
  c <- peekOne
  switch c
 where
  switch :: Char -> Parser LispExpr
  switch c
    | c `elem` digits = literalInt
    | c == '#' = EConstant <$> poundSymbols
    | otherwise = do
      h <- initialChar
      t <- some subsequentChar
      return . Variable $ h : t

constant :: Parser LispExpr
constant = empty

literalString :: Parser String
literalString = lexeme stringP

literalChar :: Parser LispExpr
literalChar =
  char '#'
    >>  char '\\'
    >>  try (continueWord "space" >> lChar <$> return ' ')
    <|> try (continueWord "newline" >> lChar <$> return '\n')
    <|> (do
          c <- lexeme asciiChar
          return $ lChar c
        )
 where
  continueWord :: String -> Parser String
  continueWord str = sequenceA (map char str)

literalInt :: Parser LispExpr
literalInt = lNum . Integral <$> L.signed spaceConsumer L.decimal

literalFloat :: Parser LispExpr
literalFloat = lNum . Floating <$> L.signed spaceConsumer L.float

vector :: Parser LispDatum
vector = do
  xs <- manyTill datum (word ")")
  if (DSymbol ".") `elem` xs
    then fail "illegal use of '.'"
    else return $ DVector xs

poundSymbols :: Parser LispDatum
poundSymbols = do
  pc@[c1, c2] <- peekPair
  case pc of
    "#t"  -> word pc >> dBool <$> return True
    "#f"  -> word pc >> dBool <$> return False
    "#b"  -> toInt pc L.binary
    "#o"  -> toInt pc L.octal
    "#x"  -> toInt pc L.hexadecimal
    "#d"  -> toInt pc L.decimal
    "#\\" -> do
      EConstant x <- literalChar
      return x
    _ -> empty
  where toInt pc fun = dNum . Integral <$> (word pc >> fun)

datum :: Parser LispDatum
datum = do
  c1 <- peekOne
  case c1 of
    '(' -> dList
    '"' -> dString <$> literalString
    '#' -> poundSymbols
    _   -> do
      sym <- symbol
      case sym of
        EConstant d    -> return d
        Variable  name -> return $ DSymbol name
        _              -> fail "Don't really know how to handle this"


-- TODO: it's wrong, lol
symbolOrFloat :: Parser LispExpr
symbolOrFloat =
  try (choice [word "+" $> Variable "+", word "-" $> Variable "-"])
    <|> literalFloat

list :: Parser LispExpr
list = do
  _  <- word "("
  xs <- manyTill expr (word ")")
  let secondToLast = last . init
  let withoutSecondToLast xss = (init . init $ xss) ++ [last xss]
  let res = if length xs > 2 && secondToLast xs == Variable "."
        then listToCons $ List (withoutSecondToLast xs)
        else List xs
  return res


dList :: Parser LispDatum
dList = do
  _  <- word "("
  xs <- manyTill datum (word ")")
  -- let secondToLast = last . init
  -- let withoutSecondToLast xss = (init . init $ xss) ++ [last xss]
  -- let res = if length xs > 2 && secondToLast xs == DSymbol "."
  --       then listToCons $ DList (withoutSecondToLast xs)
  --       else DList xs
  return $ DList xs



expander :: String -> Parser LispExpr
expander abb
  | abb `elem` abbreviations = do
    _ <- choice (map word abbreviations) -- consume abbreviation
    let abbv = M.lookup abb abbrs
    case abbv of
      Just (_, handleRest, constructor) -> constructor <$> handleRest
      Nothing                           -> expr
  | otherwise = fail "programming error"

-- this should receive a list without that magical period at the end
listToCons :: LispExpr -> LispExpr
listToCons lst = case lst of
  List els -> recCons els
  _        -> lst
 where
  recCons :: [LispExpr] -> LispExpr
  recCons []       = Variable "nil"
  recCons [x1, x2] = List [Variable "cons", x1, x2]
  recCons (x : xs) = List [Variable "cons", x, recCons xs]

-- dListToCons :: LispDatum -> LispDatum
-- dListToCons lst = case lst of
--   DList els -> recCons els
--   _        -> lst
--   where
--   recCons :: [LispDatum] -> LispDatum
--   recCons []       = DSymbol "nil"
--   recCons [x1, x2] = DList [DSymbol "cons", x1, x2]
--   recCons (x : xs) = DList [DSymbol "cons", x, recCons xs]

-- ignoring whitespace
contents :: Parser a -> Parser a
contents p = do
  spaceConsumer
  r <- p
  eof
  return r

lexExpr :: Parser LispExpr
lexExpr = contents expr

pr :: String -> IO ()
pr = parseTest lexExpr

-- abbreviation :: Context -> Parser LispExpr -> Parser LispExpr
-- abbreviation ctx p = case ctx of
--   None -> try $ do
--     c    <- oneOf "`',"
--     rest <- p

--   Quote ->

-- symbolChar :: Parser Char
-- symbolChar = alphaNumChar <|> (oneOf "!#$%&|*+-/:<=>?@^_~")

-- word :: Parser LispLexeme
-- word = do
--   _ <-

-- lexer :: Parser LispLexeme
-- lexer =
