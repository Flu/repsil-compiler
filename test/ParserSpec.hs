{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import Lexer (identifier, integerLiteral, floatLiteral)

spec :: Spec
spec = do
  describe "Lexer functionality" $ do
    it "successfully parses identifiers" $ do
      parse identifier "" "camelCase" `shouldParse` "camelCase"
      parse identifier "" "b^2+4ac" `shouldParse` "b^2+4ac"
      parse identifier "" "b!@$%^&*+=/" `shouldParse` "b!@$%^&*+=/"
      parse identifier "" "kebab-case" `shouldParse` "kebab-case"
      parse identifier "" `shouldFailOn` "-invalid-kebab"
      parse identifier "" `shouldFailOn` "3abcd"
    it "successfully parses integers" $ do
      parse integerLiteral "" "23" `shouldParse` 23
      parse integerLiteral "" "2147483647" `shouldParse` 2147483647
      parse integerLiteral "" "-2" `shouldParse` (-2)
      parse integerLiteral "" "-0" `shouldParse` 0
      parse integerLiteral "" "+7" `shouldParse` 7
      parse integerLiteral "" "-2147483648" `shouldParse` (-2147483648)
    it "successfully parses floating point notation" $ do
      parse floatLiteral "" "3.0" `shouldParse` 3.0
      parse floatLiteral "" "-2.56" `shouldParse` (-2.56)
      parse floatLiteral "" "3.12e12" `shouldParse` 3.12e12
      parse floatLiteral "" "-0.45e19" `shouldParse` (-0.45e19)
      parse floatLiteral "" `shouldFailOn` "Nan"
      parse floatLiteral "" `shouldFailOn` "Inf"
      

