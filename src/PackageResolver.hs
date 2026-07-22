{-# LANGUAGE OverloadedStrings #-}
module PackageResolver where

import Control.Monad (foldM)
import Control.Monad.Trans.Except
import Data.Text (pack, Text)
import Data.Map hiding (map)
import Data.Maybe (catMaybes)
import System.Directory
import System.FilePath
import Text.Megaparsec

import Parser
import Ast
import Error
import Control.Monad.IO.Class (MonadIO(liftIO))

type Resolve a = ExceptT CompileError IO a

data ModuleLocation = SingleFile FilePath | Package FilePath
  deriving (Show, Read, Eq, Ord)

data ModuleInfo = ModuleInfo
  {
    moduleType :: ModuleLocation,
    ast :: Module,
    depends :: [FilePath]
  }
  deriving (Show, Read, Eq, Ord)

type DependencyGraph = Map FilePath ModuleInfo

tempDG :: DependencyGraph
tempDG = Data.Map.empty

fileExtension :: String
fileExtension = ".rpsl"

getPathFromModuleLoc :: ModuleLocation -> FilePath
getPathFromModuleLoc (SingleFile fp) = fp
getPathFromModuleLoc (Package fp) = fp

directoryModuleHasDeclFile :: FilePath -> String -> IO Bool
directoryModuleHasDeclFile dirPath filename = do
  filesInDir <- listDirectory dirPath
  return (filename `elem` filesInDir)

findCandidateModule :: String -> FilePath -> Resolve (Maybe ModuleLocation)
findCandidateModule moduleName candidateDir = do
  let filePath = candidateDir </> (moduleName ++ fileExtension)
  let dirPath = candidateDir </> moduleName
  fileExists <- liftIO $ doesFileExist filePath
  directoryExists <- liftIO $  doesDirectoryExist dirPath
  case (fileExists, directoryExists) of
    (True, False) -> return $ Just (SingleFile filePath)
    (True, True) -> do
      dirHasDecl <- liftIO $ directoryModuleHasDeclFile dirPath (moduleName ++ fileExtension)
      if dirHasDecl
        then throwE $ ImportError $ AmbiguousModule moduleName [filePath, dirPath]
        else return $ Just (SingleFile filePath)
    (False, True) -> do
      dirHasDecl <- liftIO $ directoryModuleHasDeclFile dirPath (moduleName ++ fileExtension)
      if dirHasDecl
        then return $ Just (Package dirPath)
        else throwE $ ImportError $ ModuleDeclNotFound moduleName dirPath
    (False, False) -> return Nothing

wrapInCompileError :: Either ImportError ModuleLocation -> Either CompileError ModuleLocation
wrapInCompileError (Left err) = Left $ ImportError err
wrapInCompileError (Right success) = Right success

resolveModule :: String -> [FilePath] -> Resolve ModuleLocation
resolveModule moduleName candidateDirs = do
  possibleMods <- mapM (findCandidateModule moduleName) candidateDirs
  let locations = catMaybes possibleMods
  case locations of
    [] -> throwE $ ImportError $ ModuleNotFound moduleName
    [location] -> return location
    _ -> throwE $ ImportError $ AmbiguousModule moduleName (map getPathFromModuleLoc locations)

parseFileAndExtractImports :: FilePath -> Text -> Resolve (Module, [String])
parseFileAndExtractImports filePath source = do
  case parse parseModule filePath source of
    Left err -> throwE $ ParseFail err
    Right moduleAst -> do
      let imports = extractImports moduleAst
      return (moduleAst, imports)

buildDependencyGraph :: FilePath -> [FilePath] -> DependencyGraph -> String -> Resolve DependencyGraph
buildDependencyGraph cwd candidateDirs graph moduleName = do
  location <- resolveModule moduleName (cwd:candidateDirs)
  if notMember (getPathFromModuleLoc location) graph then
    case location of
      SingleFile path -> do
        source <- liftIO $ pack <$> readFile path
        (abstractSyntaxTree, importNames) <- parseFileAndExtractImports path source
        depPaths <- mapM (`resolveModule` (cwd:candidateDirs)) importNames
        let entry = ModuleInfo
              {
                moduleType = SingleFile path,
                ast = abstractSyntaxTree,
                depends = map getPathFromModuleLoc depPaths
              }
        finalGraph <- foldM (buildDependencyGraph cwd candidateDirs) (insert path entry graph) importNames
        return $ insert path entry finalGraph
        
      Package dirPath -> do
        let moduleDeclPath = dirPath </> (moduleName ++ fileExtension)
        source <- liftIO $ pack <$> readFile moduleDeclPath
        (abstractSyntaxTree, importNames) <- parseFileAndExtractImports moduleDeclPath source
        let newCwd = dirPath
        depPaths <- mapM (`resolveModule` (newCwd:candidateDirs)) importNames
        let entry = ModuleInfo
              {
                moduleType = Package dirPath,
                ast = abstractSyntaxTree,
                depends = map getPathFromModuleLoc depPaths
              }
        finalGraph <- foldM (buildDependencyGraph newCwd candidateDirs) (insert dirPath entry graph) importNames
        return $ insert dirPath entry finalGraph
    else
    return graph

type VisitationMap = Map FilePath Bool

detectCycleInDG :: FilePath -> DependencyGraph -> Resolve ()
detectCycleInDG startWithNode dg  = do
  (_, _) <- detect initVisited initRecStack startWithNode
  return ()
  where
    initVisited :: VisitationMap
    initVisited = mapWithKey (\_ _ -> False) dg
    initRecStack :: VisitationMap
    initRecStack = initVisited
    detect :: VisitationMap -> VisitationMap -> FilePath -> Resolve (VisitationMap, VisitationMap)
    detect visited recStack node = do
      case recStack ! node of
        True -> throwE (ImportError $ CircularImport node "" "")
        False -> do
          case visited ! node of
            True -> return (visited, recStack)
            False -> do
              let markVisited = Data.Map.update (\_ -> Just True) node visited
              let markOnRecStack = Data.Map.update (\_ -> Just True) node recStack
              (finalVisited, finalRecStack) <- foldM (\(v, r) nextNode -> do detect v r nextNode)
                (markVisited, markOnRecStack) (depends (dg ! node))
              let demarkOnRecStack = Data.Map.update (\_ -> Just False) node finalRecStack
              return (finalVisited, demarkOnRecStack)

testRun :: Resolve Bool
testRun = do
  let cwd = "/home/flu/Cod/repsil-project"
  absCwd <- liftIO $ makeAbsolute cwd
  graph <- buildDependencyGraph absCwd [] tempDG "repsil-project"
  detectCycleInDG "/home/flu/Cod/repsil-project/repsil-project.rpsl" graph
  liftIO $ putStrLn "The dependency graph is acyclic"
  return True
