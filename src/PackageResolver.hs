{-# LANGUAGE OverloadedStrings #-}
module PackageResolver where

import Control.Monad.Trans.Except
import qualified Data.Text as T
import Data.Map hiding (map)
import Data.Maybe (catMaybes)
import System.Directory
import System.FilePath

import Parser
import Ast
import Error

type Resolve = ExceptT CompileError IO DependencyGraph

data ModuleLocation = SingleFile FilePath | Package FilePath
  deriving (Show, Read, Eq, Ord)

data ModuleInfo = ModuleInfo
  {
    moduleType :: ModuleLocation,
    ast :: Module,
    dependencies :: [FilePath]
  }
  deriving (Show, Read, Eq, Ord)

type DependencyGraph = Map FilePath ModuleInfo

tempDG :: DependencyGraph
tempDG = empty

fileExtension :: String
fileExtension = ".rpsl"

getPathFromModuleLoc :: ModuleLocation -> FilePath
getPathFromModuleLoc (SingleFile fp) = fp
getPathFromModuleLoc (Package fp) = fp

directoryModuleHasDeclFile :: FilePath -> String -> IO Bool
directoryModuleHasDeclFile dirPath filename = do
  filesInDir <- listDirectory dirPath
  return (filename `elem` filesInDir)

findCandidateModule :: String -> FilePath -> IO (Either ImportError (Maybe ModuleLocation))
findCandidateModule moduleName candidateDir = do
  let filePath = candidateDir </> (moduleName ++ fileExtension)
  let dirPath = candidateDir </> moduleName
  fileExists <- doesFileExist filePath
  directoryExists <- doesDirectoryExist dirPath
  case (fileExists, directoryExists) of
    (True, False) -> return $ Right (Just (SingleFile filePath))
    (True, True) -> do
      dirHasDecl <- directoryModuleHasDeclFile dirPath (moduleName ++ fileExtension)
      if dirHasDecl
        then return $ Left (AmbiguousModule moduleName [filePath, dirPath])
        else return $ Right (Just (SingleFile filePath))
    (False, True) -> do
      dirHasDecl <- directoryModuleHasDeclFile dirPath (moduleName ++ fileExtension)
      if dirHasDecl
        then return $ Right (Just (Package dirPath))
        else return $ Left (ModuleDeclNotFound moduleName dirPath)
    (False, False) -> return $ Right Nothing

resolveModule :: String -> [FilePath] -> IO (Either ImportError ModuleLocation)
resolveModule moduleName candidateDirs = do
  possibleMods <- mapM (findCandidateModule moduleName) candidateDirs
  case sequence possibleMods of
    (Left err) -> return $ Left err
    (Right maybeLocations) -> do
      let locations = catMaybes maybeLocations
      case locations of
        [] -> return $ Left $ ModuleNotFound moduleName
        [location] -> return $ Right location
        _ -> return $ Left $ AmbiguousModule moduleName (map getPathFromModuleLoc locations)

buildDependencyGraph :: String -> FilePath -> [FilePath] -> Resolve
buildDependencyGraph moduleName cwd candidateDirs = do
  
  return tempDG
