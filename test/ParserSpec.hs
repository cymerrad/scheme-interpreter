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
  it "empty list" $ prs "()" `shouldParse` List []
  it "string" $ prs "\"string\"" `shouldParse` (lString "string")
  itShPrs "integer"         "1337"       (lNum (Integral 1337))
  itShPrs "boolean true"    "#t"         (lBool True)
  itShPrs "boolean false"   "#f"         (lBool False)
  itShPrs "character \\n"   "#\\newline" (lChar '\n')
  itShPrs "character space" "#\\space"   (lChar ' ')
  itShPrs "character a"     "#\\a"       (lChar 'a') -- TODO: https://hspec.github.io/quickcheck.html
  itShNotPrs "character duplicated a" "#\\aa"


abbreviationsSpec :: Spec
abbreviationsSpec = describe "Parsing abbreviations" $ do
  itShPrs "quote empty list"      "'()" (Quote (DList []))
  itShPrs "quasiquote empty list" "`()" (Quasiquote (DList []))

compoundSpec :: Spec
compoundSpec = describe "Parsing some combinations" $ do
  it "should work" $ shouldBe True True
  it "shouldn't suggest not using the 'do' notation" $ True `shouldBe` True
