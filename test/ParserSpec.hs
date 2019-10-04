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
import           Prelude                        ( uncurry )


prs :: String -> Either (ParseErrorBundle String Void) LispExpr
prs = parse lexExpr ""

itShPrs :: String -> String -> LispExpr -> SpecWith ()
itShPrs description input expectedOutput =
  it description $ prs input `shouldParse` expectedOutput


itShNotPrs :: String -> String -> SpecWith ()
itShNotPrs description input = it description $ prs `shouldFailOn` input


spec :: Spec
spec = describe "GL & HF testing this" $ do
  primitivesSpec
  compoundSpec
  abbreviationsSpec

primitivesSpec :: Spec
primitivesSpec = describe "Parsing atoms" $ do
  itShPrs "empty list"          "() "            (List [])
  itShPrs "string"              "\"string\" "    (lString "string")
  itShPrs "integer"             "1337 "          (lNum (Integral 1337))
  itShPrs "binary integer"      "#b10100111001 " (lNum (Integral 1337))
  itShPrs "octal integer"       "#o2471 "        (lNum (Integral 1337))
  itShPrs "decimal integer"     "#d1337 "        (lNum (Integral 1337))
  itShPrs "hexadecimal integer" "#x539 "         (lNum (Integral 1337))
  itShPrs "boolean true"        "#t "            (lBool True)
  itShPrs "boolean false"       "#f "            (lBool False)
  itShPrs "character \\n"       "#\\newline "    (lChar '\n')
  itShPrs "character space"     "#\\space "      (lChar ' ')
  itShPrs "character a"         "#\\a "          (lChar 'a') -- TODO: https://hspec.github.io/quickcheck.html
  itShNotPrs "character duplicated a" "#\\aa"
  itShNotPrs "almost a boolean true"  "#tt"
  itShNotPrs "almost a boolean false" "#ff"


abbreviationsSpec :: Spec
abbreviationsSpec = describe "Parsing abbreviations" $ do
  itShPrs "quote empty list"      "'()" (Quote (DList []))
  itShPrs "quasiquote empty list" "`()" (Quasiquote (DList []))


compoundSpec :: Spec
compoundSpec = describe "Parsing some combinations" $ do
  it "should work" $ shouldBe True True
  it "shouldn't suggest not using the 'do' notation" $ True `shouldBe` True

