{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module ParserFromScratchSpec
  ( spec
  )
where

import           Import
import           ParserFromScratch
import           Text.ParserCombinators.Parsec as Psc
import           Text.Parsec.Prim               ( Stream
                                                , Parsec
                                                )
import           Text.Parsec.Error              ( ParseError
                                                , newErrorMessage
                                                , addErrorMessage
                                                , Message(UnExpect, Expect)
                                                )
import           Text.Parsec.Pos                ( initialPos
                                                , incSourceColumn
                                                )
import           Test.Hspec
-- import           Test.Hspec.QuickCheck

-- spec :: Spec
-- spec = do
--   describe "plus2" $ do
--     it "basic check" $ plus2 0 `shouldBe` 2
--     it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
--     prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i

spec :: Spec
spec = describe "per parse function" $ do
  specLibIntuition
  specParseString
  specParseNumber

specLibIntuition :: Spec
specLibIntuition = do
  describe "how lib functions work" $ do
    it "" $ shouldBe True True
    it "noneOf really accepts any character besides one specified"
      $          eW (Psc.many (Psc.noneOf "\"")) allCharsExceptDoubleQuote
      `shouldBe` Right allCharsExceptDoubleQuote
    it "troubles with backslashes"
      $ eW (Psc.many ((Psc.<|>) (escaped '"') (Psc.noneOf ['"']))) ['\\', '\\']
      `shouldBe` Right "\\\\"
    describe "I'm not sure \"try\" works like I imagined" $ do
      it "try & succeed" $ eW (escaped '"') "\\\"" `shouldBe` Right '\"'
      it "try & fail"
        $          eW (escaped '"') "\\\\"
        `shouldBe` Left parseEscapedError1
      it "try, fail & succeed"
        $ eW ((Psc.<|>) (Psc.many1 $ escaped '"') (Psc.many $ Psc.char '\\'))
             "\\\\"
        `shouldBe` Right "\\\\"
      it "try, fail & try again"
        $          eW ((Psc.<|>) (escaped '"') (escaped '\\')) "\\\\"
        `shouldBe` Right '\\'

  describe "smaller utilitiy functions behave sane" $ do
    it "\"escaped\" double quote" $ eW (escaped '"') "\\\"" `shouldBe` Right '"'
    it "\"escaped\" backslash" $ eW (escaped '\\') "\\\\" `shouldBe` Right '\\'

-- TODO: monadic fold on a list of tuples
specParseString :: Spec
specParseString = describe "parseString" $ do
  it "empty" $ tFun "\"\"" `shouldBe` Right (String "")
  it "single escaped quote" $ tFun "\"\\\"\"" `shouldBe` Right (String "\\\"")
  it "double backslashes" $ tFun "\"\\\\\"" `shouldBe` Right (String "\\\\")
  -- prop "wat" $ \s -> tFun ("\"" ++ s ++ "\"") `shouldBe` Right (String s)
  where tFun = eW parseString

specParseNumber :: Spec
specParseNumber = describe "parseNumber" $ do
  it "zero" $ tFun "0" `shouldBe` Right (Number 0)
  it "NaN" $ tFun "aaa" `shouldBe` Left parseNumberError1
  where tFun = eW parseNumber

srcName :: String
srcName = "lisp_test"

-- eitherWrap
eW :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
eW p = Psc.parse p srcName

allCharsExceptDoubleQuote :: [Char]
allCharsExceptDoubleQuote =
  "\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\x7f"

parseNumberError1 :: ParseError
parseNumberError1 = addErrorMessage
  (Expect "digit")
  (newErrorMessage (UnExpect "\"a\"") (initialPos srcName))

parseEscapedError1 :: ParseError
parseEscapedError1 = addErrorMessage
  (Expect "\"\\\"\"")
  (newErrorMessage (UnExpect "\"\\\\\"")
                   (incSourceColumn (initialPos srcName) 1)
  )

