{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import Lexer (identifier)

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
