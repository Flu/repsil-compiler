module Error(ImportError(..), CompileError(..)) where

import Data.Void (Void)
import Data.Text as T
import Text.Megaparsec.Error

data CompileError =
  ImportError ImportError
  | ParseFail (ParseErrorBundle Text Void)
  deriving (Show, Eq)

data ImportError =
  AmbiguousModule String [FilePath]
  | ModuleNotFound String
  | ModuleDeclNotFound String FilePath
  | CircularImport String FilePath FilePath
  deriving (Show, Read, Eq, Ord)
