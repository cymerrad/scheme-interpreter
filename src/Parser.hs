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
import           Data.Text                      ( Text )
import           Prelude                        ( init
                                                , last
                                                )

-- type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void String a

data LispAtom
  = Atom String -- atom is just a word that __may__ be a valid token
  | List [LispAtom]
  | Symbol String
  | Quote LispAtom
  | Quasiquote LispAtom
  | Unquote LispAtom
  | Vector LispAtom
  | Cons LispAtom -- tail : head
  | LBool Bool
  | LNum Number
  | LChar Char
  | LString String
  deriving (Show, Eq)
-- append? vector?

data Number
  = Integral Int
  | Floating Float
  deriving (Show, Eq)

-- symbolChars :: String
-- symbolChars = ".!#$%&|*+-/:<=>?@^_~"

initialCharacters :: String
initialCharacters = "!$%&*/:<=>?~_^"

nonInitialCharacters :: String
nonInitialCharacters = ".+-"

initialChar :: Parser Char
initialChar = letterChar <|> oneOf initialCharacters

subsequentChar :: Parser Char
subsequentChar = initialChar <|> digitChar <|> oneOf nonInitialCharacters

abbrs :: Map String (String, Parser LispAtom, LispAtom -> LispAtom)
abbrs = M.fromList
  [ ("`" , ("quasiquote", expr, Quasiquote))
  , ("'" , ("quote", datum, Quote))
  , ("," , ("unquote", expr, Unquote))
  , ("#(", ("vector", vector, Vector))
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

expr :: Parser LispAtom
expr = do
  pairCh <- peekPair
  lexeme $ switch1 pairCh
 where
  switch1 :: String -> Parser LispAtom
  switch1 pair = case matchAbbreviation pair of
    Just abb -> expander abb
    Nothing  -> switch2 pair
  switch2 :: String -> Parser LispAtom
  switch2 [] = empty
  switch2 (ch1 : _) | ch1 == '('                      = list
                    | ch1 `elem` nonInitialCharacters = symbolOrFloat
                    | otherwise                       = symbol

peekPair :: Parser [Char]
peekPair = lookAhead $ do
  c1 <- asciiChar
  c2 <- asciiChar
  return [c1, c2]

symbol :: Parser LispAtom
symbol = do
  h <- initialChar
  t <- some subsequentChar
  return . Symbol $ h : t

constant :: Parser LispAtom
constant = empty

literalString :: Parser LispAtom
literalString = LString <$> lexeme stringP

literalFloat :: Parser LispAtom
literalFloat = LNum . Floating <$> L.signed spaceConsumer L.float

literalChar :: Parser LispAtom
literalChar = empty

-- TODO, just a placeholder for lookups
vector :: Parser LispAtom
vector = do
  xs <- manyTill datum (word ")")
  if (Symbol ".") `elem` xs then fail "illegal use of '.'" else return $ List xs

datum :: Parser LispAtom
datum = do
  pc@[c1, _] <- peekPair
  case c1 of
    '"' -> literalString
    _   -> case pc of
      "#t"  -> LBool <$> return True
      "#f"  -> LBool <$> return False
      "#b"  -> toInt pc L.binary
      "#o"  -> toInt pc L.octal
      "#x"  -> toInt pc L.hexadecimal
      "#d"  -> toInt pc L.decimal
      "#\\" -> literalChar
      _     -> empty
  where toInt = \pc fun -> LNum . Integral <$> (word pc >> fun)


symbolOrFloat :: Parser LispAtom
symbolOrFloat =
  try (choice [word "+" $> Symbol "+", word "-" $> Symbol "-"]) <|> literalFloat

list :: Parser LispAtom
list = do
  _  <- word "("
  xs <- manyTill expr (word ")")
  let secondToLast = last . init
  let withoutSecondToLast xss = (init . init $ xss) ++ [last xss]
  let res = if length xs > 2 && secondToLast xs == Atom "."
        then listToCons $ List (withoutSecondToLast xs)
        else List xs
  return res

expander :: String -> Parser LispAtom
expander abb
  | abb `elem` abbreviations = do
    _ <- choice (map word abbreviations) -- consume abbreviation
    let abbv = M.lookup abb abbrs
    case abbv of
      Just (_, handleRest, constructor) -> constructor <$> handleRest
      Nothing                           -> expr
  | otherwise = fail "programming error"

-- this should receive a list without that magical period at the end
listToCons :: LispAtom -> LispAtom
listToCons lst = case lst of
  List els -> recCons els
  _        -> lst
 where
  recCons :: [LispAtom] -> LispAtom
  recCons []       = Symbol "nil"
  recCons [x1, x2] = List [Symbol "cons", x1, x2]
  recCons (x : xs) = List [Symbol "cons", x, recCons xs]

-- ignoring whitespace
contents :: Parser a -> Parser a
contents p = do
  spaceConsumer
  r <- p
  eof
  return r

lexExpr :: Parser LispAtom
lexExpr = contents expr

pr :: String -> IO ()
pr = parseTest lexExpr

-- abbreviation :: Context -> Parser LispAtom -> Parser LispAtom
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
