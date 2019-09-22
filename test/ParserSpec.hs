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


prs :: Text -> Either (ParseErrorBundle Text Void) LispAtom
prs = parse lexExpr ""

itShPrs description input expectedOutput =
  it description $ prs input `shouldParse` expectedOutput

spec :: Spec
spec = describe "GL & HF testing this" $ do
  primitivesSpec
  combinationsSpec

primitivesSpec :: Spec
primitivesSpec = describe "Parsing atoms" $ do
  it "empty list" $ prs "()" `shouldParse` List []
  it "string" $ prs "\"string\"" `shouldParse` Constant (String "string")
  itShPrs "integer" "1337" (Constant . Number . Integral 1337)

combinationsSpec :: Spec
combinationsSpec = describe "Parsing some combinations" $ do
  it "should work" $ shouldBe True True
