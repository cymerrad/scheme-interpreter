{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserFromScratch where

import           Text.Parsec
import           RIO                     hiding ( (<|>)
                                                , many
                                                , try
                                                )
import           Prelude                        ( read )

type Parser = Parsec String ()

-- data LispNumber
--   = Integer Integer
--   | Hex Integer
--   | Octal Integer
--   | Fraction Rational
--   | Float Double
--   deriving (Show, Eq)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
  deriving (Show, Eq)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces' :: Parser ()
spaces' = skipMany1 space

escaped :: Char -> Parser Char
escaped ch = try
  (do
    _ <- char '\\' -- consume the slash character entirely
    char ch -- return what's supposed to be escaped
  )

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  -- x <- many (escaped '"' <|> noneOf ['"'])
  x <-
    try
        (do -- if reading '"' succeeds, return one empty string
          _ <- char '"'
          return [""]
        )
      <|> (do -- else read these, being wary of escaped quotes
            xs <-
              many1
              $ -- non-empty string
                  try -- try reading escaped backslash
                    (do
                      escQ <- string "\\\""
                      notFollowedBy eof -- if reached end of input, then fail
                      -- let the next phase to read the backslash and then the quote
                      return escQ
                    )
              <|> many1 (noneOf ['"'])

            _ <- char '"' -- this marks the end
            return xs
          )
  _ <- eof -- nothing must follow afterwards
  return $ String $ concat x

parseNumber :: Parser LispVal
parseNumber = do
  x <- many1 digit
  return $ Number $ read x


parseAtom :: Parser LispVal
parseAtom = do
  head <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = head : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> case val of
    String str -> "Found string: " ++ str
    _          -> "Found value: " ++ show val
