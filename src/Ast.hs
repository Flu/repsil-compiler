module Ast where

import Data.Text
import Lexer (Span(..))

data Identifier = Identifier Text Text

newtype Atom = Atom Text

data Val =
  VInt Int
  | VFloat Float
  | VText Text
  | VBool Bool
  | VChar Char
  | VList Expr

data Defun =
  Defun Span Identifier Atom [(Val, Identifier)] Expr

data Expr =
  FunCallExpr Span Identifier [Expr]
  | PrognExpr Span [Expr]
  | IfExpr Span Expr Expr Expr
  | LetExpr Span [(Atom, Identifier, Expr)] Expr
  | VarExpr Span Identifier
  | LitExpr Span Val

data Defvar =
  Defvar Span Atom Identifier Expr

data ImportDir =
  ImportDir Span Atom

data ExportList =
  ExportList Span [Identifier]

data TopLevel =
  Imports [ImportDir]
  | Exports [ExportList]
  | Vars [Defvar]
  | Funs [Defun]

type Module = [TopLevel]
