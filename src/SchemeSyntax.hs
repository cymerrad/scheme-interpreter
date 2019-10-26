{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemeSyntax where
import           RIO

data Form
  = Definition Def
  | Expression Expr

data Def
  = VariableDefinition VarDef -- (define <...>)
  | DefineSyntax Identifier TransExpr -- (define-syntax <...>)
  | Begin Def -- (begin <definition>)
  | LetSyntax SyntaxBind Def -- (let-syntax <...>)
  | LetrecSyntax SyntaxBind Def -- (letrec-syntax <...>)


data VarDef
  = Define1 Variable
  | DefineMany [Variable] Body
  -- Cons!

type Identifier = String

data Body
  = Body [Def] [Expr]

data SyntaxBind = SyntaxBind Identifier TransExpr -- (<keyword> <transformer expression>)

newtype TransExpr = SyntaxRules String -- this branch is the dead end; I don't know what it is

newtype Variable = V String

data Expr
  = EConstant Constant --
  | EVariable Variable --
  | Quote Datum -- (quote <datum>) | ' <datum>

data Datum
  = DConstant Constant
  | DSymbol Identifier
  | DList List
  | DVector [Datum] -- #()

data Constant
  = CBool Bool
  | CNum Number
  | CChar Char
  | CString String
  deriving (Show, Eq)

data Number
  = Integral Int
  | Floating Float
  deriving (Show, Eq)

data List
  = List [Datum]
  | Cons [Datum] Datum
  | LAbbr Abbreviation

data Abbreviation
  = AQuote Datum -- '
  | AQuasiquote Datum -- `
  | AUnquote Datum -- ,
  | AUnquoteAppend Datum -- ,@

eNumInt = EConstant . CNum . Integral
eNumFl = EConstant . CNum . Floating
eBool = EConstant . CBool
eChar = EConstant . CChar
eString = EConstant . CString
