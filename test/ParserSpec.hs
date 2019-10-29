{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserSpec
  ( spec
  )
where

import           Import
import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec


prs :: String -> Either (ParseErrorBundle String Void) LispExpr
prs = parse parseExpr ""

itShPrs :: String -> String -> LispExpr -> SpecWith ()
itShPrs description input expectedOutput =
  it description $ prs input `shouldParse` expectedOutput


itShNotPrs :: String -> String -> SpecWith ()
itShNotPrs description input = it description $ prs `shouldFailOn` input


spec :: Spec
spec = describe "GL & HF testing this" $ do
  primitivesSpec
  abbreviationsSpec
  compoundSpec

primitivesSpec :: Spec
primitivesSpec = describe "Parsing atoms" $ do
  itShPrs "empty list"          "() "            (List [])
  itShPrs "string"              "\"string\" "    (lString "string")
  itShPrs "integer"             "1337 "          (lNum (Integral 1337))
  itShPrs "signed integer"      "-2137 "         (lNum (Integral (-2137)))
  itShPrs "binary integer"      "#b10100111001 " (lNum (Integral 1337))
  itShPrs "octal integer"       "#o2471 "        (lNum (Integral 1337))
  itShPrs "decimal integer"     "#d1337 "        (lNum (Integral 1337))
  itShPrs "hexadecimal integer" "#x539 "         (lNum (Integral 1337))
  itShPrs "floating"            "1.0 "           (lNum (Floating 1.0))
  itShPrs "signed floating"     "-1.0 "          (lNum (Floating (-1.0)))
  itShPrs "boolean true"        "#t "            (lBool True)
  itShPrs "boolean false"       "#f "            (lBool False)
  itShPrs "character \\n"       "#\\newline "    (lChar '\n')
  itShPrs "character space"     "#\\space "      (lChar ' ')
  itShPrs "character \\"        "#\\\\"          (lChar '\\')
  itShPrs "character a"         "#\\a "          (lChar 'a') -- TODO: https://hspec.github.io/quickcheck.html
  itShNotPrs "character duplicated a" "#\\aa"
  itShNotPrs "almost a boolean true"  "#tt"
  itShNotPrs "almost a boolean false" "#ff"


abbreviationsSpec :: Spec
abbreviationsSpec = describe "Parsing abbreviations" $ do
  itShPrs "quote empty list"      "'()"      (Quote (List []))
  itShPrs "quasiquote empty list" "`()"      (Quasiquote (List []))
  itShPrs "unquote empty list"    ",()"      (Unquote (List [])) -- shouldn't be possible at level above quote
  itShPrs "quote list"            "'(+ 1 2)" (Quote (List [Symbol "+",Constant (LNum (Integral 1)),Constant (LNum (Integral 2))]))
  itShPrs "quasiquote list"       "`(+ 1 2)" (Quasiquote (List [Symbol "+",Constant (LNum (Integral 1)),Constant (LNum (Integral 2))]))
  itShPrs "unquote list"          ",(+ 1 2)" (Unquote (List [Symbol "+",Constant (LNum (Integral 1)),Constant (LNum (Integral 2))]))

compoundSpec :: Spec
compoundSpec = describe "Parsing some combinations" $ do
  it "should work" $ shouldBe True True
  it "shouldn't suggest not using the 'do' notation" $ True `shouldBe` True
  itShPrs "addition"
          "(+ 1 2)"
          (List [Symbol "+", lNum (Integral 1), lNum (Integral 2)])
  itShPrs "subtraction"
          "(- 1 2)"
          (List [Symbol "-", lNum (Integral 1), lNum (Integral 2)])
  itShPrs
    "lots of whitespace around"
    "\t(   - (+\n 1    1)    2 )     "
    (List
      [ Symbol "-"
      , List [Symbol "+", lNum (Integral 1), lNum (Integral 1)]
      , lNum (Integral 2)
      ]
    )
  itShPrs
    "lots of mixed up symbols"
    "(()  \"string\" 1337  -2137  #b10100111001  #o2471  #d1337  #x539  1.0  -1.0  #t  #f  #\\newline  #\\\\ #\\space  #\\a )"
    (List
      [ List []
      , lString "string"
      , lNum (Integral 1337)
      , lNum (Integral (-2137))
      , lNum (Integral 1337)
      , lNum (Integral 1337)
      , lNum (Integral 1337)
      , lNum (Integral 1337)
      , lNum (Floating 1.0)
      , lNum (Floating (-1.0))
      , lBool True
      , lBool False
      , lChar '\n'
      , lChar '\\'
      , lChar ' '
      , lChar 'a'
      ]
    )
  itShPrs "Zermelo 1" "(())"      (List [List []])
  itShPrs "Zermelo 2" "((()) ())" (List [List [List []], List []])
  itShPrs
    "quasiquote unquote"
    "`(,(+ 1 2))"
    (Quasiquote (List [Unquote (List [Symbol "+", lNum (Integral 1), lNum (Integral 2)])]))

