{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char (char)
import qualified Data.Text as T
import Ast
import Lexer

pValue :: Parser Val
pValue = try (VInt <$> integerLiteral)
         <|> (VFloat <$> floatLiteral)
         <|> (VString <$> stringLiteral)
         <|> (VChar <$> charLiteral)
         <|> (VBool <$> boolLiteral)
         -- <|> parseList

pAtom :: Parser Atom
pAtom = Atom <$> (T.cons <$> char ':' *> identifier)

pInteger :: Parser Val
pInteger = VInt <$> integerLiteral

pFloat :: Parser Val
pFloat = VFloat <$> floatLiteral

pString :: Parser Val
pString = VString <$> stringLiteral

pBool :: Parser Val
pBool = VBool <$> boolLiteral

pChar :: Parser Val
pChar = VChar <$> charLiteral

pIdentifier :: Parser Identifier
pIdentifier = do
  parts <- namespaces
  return $ case parts of
    [x] -> Identifier [] x
    qualified_name -> Identifier (init qualified_name) (last qualified_name)
  where
    namespaces = (:) <$> identifier <*> many (string "::" *> identifier)

pDefunArg :: Parser (Atom, Identifier)
pDefunArg = parens ((,) <$> pAtom <*> pIdentifier)

pDefun :: Parser Defun
pDefun = withSpan defunWithoutSpan
  where
    defunWithoutSpan = parens (Defun
                               <$> (reserved "defun" *> pIdentifier)
                               <*> pAtom
                               <*> parens (many pDefunArg)
                               <*> withSpan (PrognExpr <$> many pExpression))

pDefvar :: Parser Defvar
pDefvar = withSpan $ parens (Defvar <$> (reserved "defvar" *> pAtom) <*> pIdentifier <*> pExpression)

pExpression :: Parser Expr
pExpression = try pPrognExpr
              <|> try pIfExpr
              <|> try pLetExpr
              <|> try pFunCallExpr
              <|> try pLitExpr
              <|> pVarExpr

pPrognExpr :: Parser Expr
pPrognExpr = withSpan $ parens $ PrognExpr <$> (reserved "progn" *> many pExpression)

pIfExpr :: Parser Expr
pIfExpr = withSpan $ parens $ IfExpr <$> (reserved "if" *> pExpression) <*> pExpression <*> pExpression

pFunCallExpr :: Parser Expr
pFunCallExpr = withSpan $ parens $ FunCallExpr <$> pIdentifier <*> many pExpression

pLetBindings :: Parser [(Atom, Identifier, Expr)]
pLetBindings = many (parens ((,,) <$> pAtom <*> pIdentifier <*> pExpression))

pLetExpr :: Parser Expr
pLetExpr = withSpan $ parens $ LetExpr <$> (reserved "let" *> parens pLetBindings) <*> pExpression

pVarExpr :: Parser Expr
pVarExpr = withSpan $ VarExpr <$> pIdentifier

pLitExpr :: Parser Expr
pLitExpr = withSpan $ LitExpr <$> try (pInteger
                            <|> try pFloat
                            <|> try pString
                            <|> try pBool
                            <|> pChar)

pImportDir :: Parser ImportDir
pImportDir = withSpan $ parens $ ImportDir <$ reserved "defimport" <*> pAtom

pExportList :: Parser ExportList
pExportList = withSpan $ parens $ ExportList <$ reserved "defpackage" <*> many pIdentifier

parseModule :: Parser Module
parseModule = do
  sc
  stmts <- many pTopLevel
  eof
  return stmts
  where
    pTopLevel = try (Fun <$> pDefun)
      <|> try (Var <$> pDefvar)
      <|> try (Import <$> pImportDir)
      <|> (Export <$> pExportList)
