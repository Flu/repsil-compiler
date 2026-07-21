{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import Lexer (Span(..), identifier, integerLiteral, floatLiteral)
import Parser
import Ast
import Parser (parseModule)

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
  describe "Parser functionality" $ do
    it "can parse atoms" $ do
      parse pAtom "" ":some-atom" `shouldParse` Atom "some-atom"
    it "can parse identifiers" $ do
      parse pIdentifier "" "namespace::id" `shouldParse` Identifier ["namespace"] "id"
      parse pIdentifier "" "std::list::append" `shouldParse` Identifier ["std", "list"] "append"
      parse pIdentifier "" "std::Object::class" `shouldParse` Identifier ["std", "Object"] "class"
      parse pIdentifier "" "normal-identifier" `shouldParse` Identifier [] "normal-identifier"
      parse pIdentifier "" "separate-id :atom" `shouldParse` Identifier [] "separate-id"
    it "can parse imports and exports" $ do
      parse pImportDir "" "(defimport :some-package)"
        `shouldParse` ImportDir (Atom "some-package") (Span 0 25)
      parse pExportList "" "(defpackage mod::func mod::func2 util)"
        `shouldParse` ExportList [
        Identifier ["mod"] "func",
        Identifier ["mod"] "func2",
        Identifier [] "util"
        ] (Span 0 38)
    it "can parse a syntactically correct program" $ do
      source <- readFile "test/test_files/complete_program.rpsl"
      parse parseModule "" `shouldSucceedOn` T.pack source
    it "can parse a module declaration file" $ do
      source <- readFile "test/test_files/module_decl.rpsl"
      parse parseModule "" `shouldSucceedOn` T.pack source
