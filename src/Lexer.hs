{-# LANGUAGE OverloadedStrings #-}
module Lexer(identifier, reserved, withSpan, symbol) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data ParserError =
  ReservedWordUsed Text
  | ReservedIdentifier Text
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParserError where
  showErrorComponent (ReservedWordUsed t) = "reserved word used as identifier: " <> T.unpack t
  showErrorComponent (ReservedIdentifier t) = "this identifier name is reserved: " <> T.unpack t

type Parser = Parsec ParserError Text

data Span = Span Int Int deriving (Eq, Show)

reservedWords :: [Text]
reservedWords = ["let", "if", "defun", "export", "defimport"]

identStartChars :: Parser Char
identStartChars = letterChar <|> oneOf ("+*/^@$%&<>=!?_" :: String)

identFollowChars :: Parser Char
identFollowChars = identStartChars <|> char '-' <|> digitChar

withSpan :: Parser a -> Parser (a, Span)
withSpan p = do
  start <- getOffset
  x <- p
  end <- getOffset
  return (x, Span start end)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rawIdentifier :: Parser Text
rawIdentifier = T.pack <$> ((:) <$> identStartChars <*> many identFollowChars)

identifier :: Parser Text
identifier = lexeme $ try $ do
  name <- rawIdentifier
  if name `elem` reservedWords
    then customFailure (ReservedWordUsed name)
    else pure name

reserved :: Text -> Parser ()
reserved kw = lexeme $ try $ string kw *> notFollowedBy identFollowChars
