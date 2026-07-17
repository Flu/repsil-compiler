module Ast where

import Data.Text

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
  Defun Identifier Atom [(Val, Identifier)] Expr

data Expr =
  FunCallExpr Identifier [Expr]
  | PrognExpr [Expr]
  | IfExpr Expr Expr Expr
  | LetExpr [(Atom, Identifier, Expr)] Expr
  | VarExpr Identifier
  | LitExpr Val

data Defvar =
  Defvar Atom Identifier Expr

newtype ImportDir =
  ImportDir Atom

newtype ExportList =
  ExportList [Identifier]

data TopLevel =
  Imports [ImportDir]
  | Exports [ExportList]
  | Vars [Defvar]
  | Funs [Defun]

type Module = [TopLevel]
