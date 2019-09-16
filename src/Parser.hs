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
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Prelude                        ( init
                                                , last
                                                )

-- type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void Text a

data LispAtom
  = Atom String -- atom is just a word that __may__ be a valid token
  | List [LispAtom]
  | Symbol String
  | Constant Literal
  deriving (Show, Eq)

data Literal
  = Boolean Bool
  | Number
  | Character String
  | String String
  deriving (Show, Eq)

data Number
  = Integral Int
  | Floating Float
  deriving (Show, Eq)

data Context = None | Quote | Quasiquote | Unquote | Cons | Append deriving (Show, Eq)

symbolChars :: String
symbolChars = ".!#$%&|*+-/:<=>?@^_~"

initialChar :: Parser Char
initialChar = letterChar <|> oneOf ("!$%&*/:<=>?~_^" :: String)

subsequentChar :: Parser Char
subsequentChar = initialChar <|> digitChar <|> oneOf (".+-" :: String)

abbrs :: Map String String
abbrs = M.fromList [("`", "quasiquote"), ("'", "quote"), (",", "unquote")]

abbreviations :: [String]
abbreviations = M.keys abbrs -- ["`", "'", ","]

abbreviationsT :: [Text]
abbreviationsT = map pack abbreviations

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

word :: Text -> Parser Text
word = L.symbol spaceConsumer

stringP :: Parser String
stringP = char '\"' *> manyTill L.charLiteral (char '\"')

parens :: Parser a -> Parser a
parens = between (word "(") (word ")")

expr :: Parser LispAtom
expr = do
  pairCh <- peekPair
  switch1 pairCh
 where
  switch1 :: String -> Parser LispAtom
  switch1 pair = case matchAbbreviation pair of
    Just abb -> expander abb expr
    Nothing  -> switch2 pair
  switch2 :: String -> Parser LispAtom
  switch2 [] = empty
  switch2 pair@(ch1 : _) | -- go from most specific to least
                           pair == "#("           = vector
                         | ch1 == '('             = list
                         | ch1 == '#'             = constant
                         | ch1 `elem` symbolChars = symbol
                         | ch1 == '"'             = literalString
                         | otherwise              = symbol

  peekPair :: Parser [Char]
  peekPair = lookAhead $ do
    c1 <- asciiChar
    c2 <- asciiChar
    return [c1, c2]

symbol :: Parser LispAtom
symbol = do
  x <- lexeme sym
  return $ Atom x
  where sym = some (alphaNumChar <|> oneOf symbolChars)

constant :: Parser LispAtom
constant = empty

literalString :: Parser LispAtom
literalString = Constant . String <$> lexeme stringP

-- TODO, just a placeholder for lookups
vector :: Parser LispAtom
vector = empty

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

expander :: String -> Parser LispAtom -> Parser LispAtom
expander abb p
  | abb `elem` abbreviations = do
    _    <- choice (map word abbreviationsT) -- consume
    rest <- p
    let abbv = M.lookup abb abbrs
    return $ case abbv of
      Just expanded -> List [Atom expanded, rest]
      Nothing       -> rest
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

pr :: Text -> IO ()
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
