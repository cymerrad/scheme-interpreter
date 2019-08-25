{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module ParserSpec
  ( spec
  )
where

import           Import
import           Parser
import           Test.Hspec


spec :: Spec
spec = describe "GL & HF testing this" $ do
  it "fml" $ shouldBe True True
