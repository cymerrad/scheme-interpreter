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


prs :: String -> Either (ParseErrorBundle String Void) LispAtom
prs = parse lexExpr ""

itShPrs :: String -> String -> LispAtom -> SpecWith ()
itShPrs description input expectedOutput =
  it description $ prs input `shouldParse` expectedOutput

spec :: Spec
spec = describe "GL & HF testing this" $ do
  primitivesSpec
  compoundSpec
  abbreviationsSpec

primitivesSpec :: Spec
primitivesSpec = describe "Parsing atoms" $ do
  it "empty list" $ prs "()" `shouldParse` List []
  it "string" $ prs "\"string\"" `shouldParse` (LString "string")
  itShPrs "integer"         "1337"       (LNum (Integral 1337))
  itShPrs "boolean true"    "#t"         (LBool True)
  itShPrs "boolean false"   "#f"         (LBool False)
  itShPrs "character \\n"   "#\\newline" (LChar '\n')
  itShPrs "character space" "#\\space"   (LChar ' ')
  itShPrs "character a"     "#\\a"       (LChar 'a') -- TODO: https://hspec.github.io/quickcheck.html


abbreviationsSpec :: Spec
abbreviationsSpec = describe "Parsing abbreviations" $ do
  itShPrs "quote empty list"      "'()" (Quote (List []))
  itShPrs "quasiquote empty list" "`()" (Quasiquote (List []))

compoundSpec :: Spec
compoundSpec = describe "Parsing some combinations" $ do
  it "should work" $ shouldBe True True
  it "shouldn't suggest not using the 'do' notation" $ True `shouldBe` True
