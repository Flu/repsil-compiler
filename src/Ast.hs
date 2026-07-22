module Ast where

import Data.Text (Text, unpack)
import Lexer (Span(..))

data Identifier = Identifier [Text] Text
  deriving (Ord, Eq, Show, Read)

newtype Atom = Atom Text
  deriving (Ord, Eq, Show, Read)

data Val =
  VInt Int
  | VFloat Float
  | VString Text
  | VBool Bool
  | VChar Char
  | VList Expr
  deriving (Ord, Eq, Show, Read)

data Defun =
  Defun Identifier Atom [(Atom, Identifier)] Expr Span
  deriving (Ord, Eq, Show, Read)

data Expr =
  FunCallExpr Identifier [Expr] Span
  | PrognExpr [Expr] Span
  | IfExpr Expr Expr Expr Span
  | LetExpr [(Atom, Identifier, Expr)] Expr Span
  | VarExpr Identifier Span
  | LitExpr Val Span
  deriving (Ord, Eq, Show, Read)

data Defvar =
  Defvar Atom Identifier Expr Span
  deriving (Ord, Eq, Show, Read)

data ImportDir =
  ImportDir Atom Span
  deriving (Ord, Eq, Show, Read)

data ExportList =
  ExportList [Identifier] Span
  deriving (Ord, Eq, Show, Read)

data TopLevel =
  Import ImportDir
  | Export ExportList
  | Var Defvar
  | Fun Defun
  deriving (Ord, Eq, Show, Read)

type Module = [TopLevel]

-- Helper functions for managing the AST
extractImports :: Module -> [String]
extractImports moduleAst = map (unpack . extract) filteredImports
  where
    isImport (Import _) = True
    isImport _ = False
    filteredImports = filter isImport moduleAst
    extract (Import (ImportDir (Atom sym) _)) = sym
    extract _ = error "Unreachable condition"
