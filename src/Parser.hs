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
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )
import           Prelude                        ( init
                                                , last
                                                )

-- type Parsec e s a = ParsecT e s Identity a

data LispAtom
  = Atom String
  | List [LispAtom]
  deriving (Show, Eq)

data Context = None | Quote | Quasiquote | Unquote | Cons | Append deriving (Show, Eq)

symbolChars :: String
symbolChars = ".!#$%&|*+-/:<=>?@^_~"

abbreviationChars :: [Char]
abbreviationChars = ['`', '\'', ',']

type Parser a = Parsec Void Text a

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

word :: Text -> Parser Text
word = L.symbol spaceConsumer

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

parens :: Parser a -> Parser a
parens = between (word "(") (word ")")

expr :: Parser LispAtom
expr = do
  c <- lookAhead asciiChar
  switch c
 where
  switch :: Char -> Parser LispAtom
  switch ch | ch `elem` abbreviationChars = expander ch expr
            | ch == '('                   = list
            | ch `elem` symbolChars       = symbol
            | otherwise                   = symbol

symbol :: Parser LispAtom
symbol = do
  x <- lexeme sym
  return $ Atom x
  where sym = some (alphaNumChar <|> oneOf symbolChars)

list :: Parser LispAtom
list = do
  _  <- word "("
  xs <- manyTill expr (word ")")
  let secondToLast = last . init
  let withoutSecondToLast xss = (init . init $ xss) ++ [last xss]
  let res = if length xs > 2 && secondToLast xs == (Atom ".")
        then listToCons $ List (withoutSecondToLast xs)
        else List xs
  return res

expander :: Char -> Parser LispAtom -> Parser LispAtom
expander ch p
  | ch `elem` abbreviationChars = do
    _    <- oneOf abbreviationChars -- consume ch
    rest <- p
    return $ case ch of
      '`'  -> List [Atom "quasiquote", rest]
      '\'' -> List [Atom "quote", rest]
      ','  -> List [Atom "unquote", rest]
      _    -> rest
  | otherwise = fail "programming error"

-- this should receive a list without that magical period at the end
listToCons :: LispAtom -> LispAtom
listToCons lst = case lst of
  Atom _   -> lst
  List els -> recCons els
 where
  recCons :: [LispAtom] -> LispAtom
  recCons []       = Atom "nil"
  recCons [x1, x2] = List [Atom "cons", x1, x2]
  recCons (x : xs) = List [Atom "cons", x, recCons xs]

-- ignoring whitespace
contents :: Parser a -> Parser a
contents p = do
  spaceConsumer
  r <- p
  eof
  return r

lexExpr :: Parser LispAtom
lexExpr = contents expr

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
