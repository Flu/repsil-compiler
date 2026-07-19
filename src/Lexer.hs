{-# LANGUAGE OverloadedStrings #-}
module Lexer(
  Span(..),
  Parser,
  sc,
  identifier,
  reserved,
  withSpan,
  symbol,
  integerLiteral,
  floatLiteral,
  charLiteral,
  stringLiteral,
  boolLiteral,
  parens) where

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

data Span = Span Int Int
  deriving (Ord, Eq, Show, Read)

reservedWords :: [Text]
reservedWords = ["let", "if", "defun", "export", "defimport", "true", "false"]

identStartChars :: Parser Char
identStartChars = letterChar <|> oneOf ("+*/^@$%&<>=!?_" :: String)

identFollowChars :: Parser Char
identFollowChars = identStartChars <|> char '-' <|> digitChar

withSpan :: Parser (Span -> a) -> Parser a
withSpan p = do
  start <- getOffset
  x <- p
  end <- getOffset
  return (x (Span start end))

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
  
natural :: Parser Int
natural = lexeme L.decimal

integerLiteral :: Parser Int
integerLiteral = L.signed sc natural

floatLiteral :: Parser Float
floatLiteral = L.signed sc (lexeme L.float)

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

boolLiteral :: Parser Bool
boolLiteral = try (True <$ reserved "true") <|> (False <$ reserved "false")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
