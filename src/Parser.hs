{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           RIO                     hiding ( (<|>) )
import           Text.Parsec                   as P
import qualified Text.Parsec.Token             as PT
import qualified Text.Parsec.Language          as PL
import           Text.Parsec.String             ( Parser )
import           Prelude                        ( read )

-- note for later about pound symbol
-- https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Additional-Notations.html

-- type Parser = P.Parsec String ()

-- TODO
-- Warning:
-- this is a lexer and parser merged into one (bad design, ik)

-- syntax
data LispVal = Symbol String
             | Variable String
             | Lit Lit
             | List [LispVal]
             | DotNotation

  deriving (Show, Eq)

data Lit
  = LInt Integer
  | LString String
  | LBool Bool
  deriving (Show, Eq)


_abbrChars :: [Char]
_abbrChars = ['.', '`', '\'', ','] -- and few others?

emptyLan :: PL.LanguageDef st
emptyLan = PL.emptyDef
language :: PL.GenLanguageDef String u Identity
language = emptyLan
  { PT.identStart      = P.letter <|> P.oneOf "!#$%&|*+-/:<=>?@^_~"
  , PT.identLetter     = P.alphaNum <|> P.oneOf "!#$%&|*+-/:<=>?@^_~"
  , PT.caseSensitive   = False
  , PT.reservedOpNames = map (: []) _abbrChars
  , PT.reservedNames   = ["#f", "#t", "quote", "lambda", "set!", "if", "cons"]
  }

lexer :: PT.GenTokenParser String u Identity
lexer = PT.makeTokenParser language

parens :: Parser a -> Parser a
parens = PT.parens lexer

whiteSpace :: Parser ()
whiteSpace = PT.whiteSpace lexer

reserved :: String -> Parser ()
reserved = PT.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = PT.reservedOp lexer

identifier :: Parser String
identifier = PT.identifier lexer

string' :: Parser String
string' = PT.stringLiteral lexer

-- ignoring whitespace
contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  P.eof
  return r

{- via https://www.scheme.com/tspl2d/grammar.html -}

{- DEFINITIONS -}
variable :: Parser LispVal
variable = Variable <$> identifier

{- EXPRESSIONS -}
expression :: Parser LispVal
-- expression = nil <|> variable <|> quote <|> constant <|> application
    -- <|> lambda
    -- <|> if'
    -- <|> set'
expression = do
  whiteSpace
  c <- P.lookAhead P.anyChar
  case c of
    '(' -> nil <|> application
    _   -> abbreviation <|> constant <|> variable

-- lambda :: Parser LispVal
-- lambda = parens -- formals body?


-- another idea: "try" for reserved words, some of which will be booleans
constant :: Parser LispVal
constant = stringLit <|> poundLit <|> number

-- formals :: Parser LispVal

application :: Parser LispVal
application = parens $ do
  x  <- coreExpr <|> expression
  -- TODO case on type of x
  xs <- P.many expression
  return $ List $ x : xs

nil :: Parser LispVal
nil = do
  _ <- P.try $ P.string "()" -- TODO not true, there can be spaces inside
  return $ List []

abbreviation :: Parser LispVal
abbreviation = do
  c <- P.try $ P.oneOf _abbrChars
  case c of
    '\'' -> quote
    -- '.'  -> cons -- period is not an abbreviation
    _    -> P.unexpected "too lazy to implement rest atm"

-- like quote or lambda
coreExpr :: Parser LispVal
coreExpr = P.try $ do
  someId <- identifier
  _      <- P.parserTrace $ "identified " ++ someId
  _      <- reserved someId
  unexpected "kthxbai"

{- IDENTIFIERS -}
quote :: Parser LispVal
quote = do
  _ <- P.try $ P.char '\''
  d <- datum
  return $ List [Symbol "quote", d] -- sth like this?

{- DATA -}
datum :: Parser LispVal
datum = nil <|> consOp <|> constant <|> symbol <|> list -- <|> vector <|> character

symbol :: Parser LispVal
symbol = Symbol <$> identifier

-- TODO expanding definition of list to the specification, solves the case of " '('aaa . 'bbb) "
list :: Parser LispVal
list = parens $ do
  xs <- P.many1 datum
  return $ List xs

-- (cons 'a (cons 'b (cons 'c 'd))) == (cons 'a (cons 'b '(c . d))) == (cons 'a '(b c . d)) == '(a b c . d)

-- TODO: this can happen only inside a datum or formals
consOp :: Parser LispVal
consOp = do
  _ <- P.try $ string "."
  return DotNotation

boolean :: Parser LispVal
boolean = do
  b <-
    do
        P.try $ reserved "#f"
        return False
      <|> do
            P.try $ reserved "#t"
            return True
  return $ Lit $ LBool b

poundLit :: Parser LispVal
poundLit = do
  _ <- P.lookAhead $ P.try $ P.char '#'
  boolean <|> symbol --  <|> character

number :: Parser LispVal
number = do
  d  <- P.try P.digit
  -- case c of
  --   '#' -> do
    -- ...
  -- radix <-
  --   P.string "#b"
  --   <|> P.string "#o"
  --   <|> P.string "#d"
  --   <|> P.string "#h"
  --   <|> P.string ""
  ds <- P.many1 P.digit
  -- frac <- ignore for now
  return . Lit . LInt . read $ d : ds

stringLit :: Parser LispVal
stringLit = do
  _ <- P.lookAhead $ P.try $ P.char '"'
  Lit . LString <$> string'

-- PARSE

parseExpr :: Parser LispVal
parseExpr = contents expression

readExpr :: String -> Either P.ParseError LispVal
readExpr = P.parse parseExpr "<stdin>"

