module Lib where

import Types
import JSONInstances
import AWSCommands
import TreeUtils

import Data.Tree
import Data.Tree.Pretty
import Data.Maybe
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson

import Debug.Trace

showDeletionPlan :: String -> IO ()
showDeletionPlan stackName = do
  putStrLn $ "Retrieving dependencies of " ++ stackName ++ "..."
  tree <- buildDependencyGraph (StackName stackName)
  putStrLn "Done.  Deletion order:\n"
  putStrLn $ drawTree (dependencyToTree tree)
  putStrLn "Or, delete manually in this order:\n"
  mapM_ putStrLn $ postorder (dependencyToTree tree)
  putStrLn "\nIf you trust this app you can execute:"
  putStrLn $ "forest-fire \"" ++ stackName ++ "\" --delete\n"

actuallyDoTheDelete :: String -> IO ()
actuallyDoTheDelete stackName = do
  putStrLn $ "Retrieving dependencies of " ++ stackName ++ "..."
  tree <- buildDependencyGraph (StackName stackName)
  putStrLn "Done.  Deletion order:\n"
  putStrLn $ drawTree (dependencyToTree tree)
  putStrLn "Deleting dependencies and stack..."
  mapM_ (doDeletionWait . StackName) $ postorder (dependencyToTree tree)

class Monad m => AWSExecution m where
  findExportsByStack :: StackName -> m [ExportName]
  whoImportsThisValue :: ExportName -> m [StackName]

instance AWSExecution IO where
  findExportsByStack s = do
    json <- eitherDecode <$> jsonForDescribeStacks s
    either error (pure . map eName . concatMap sExports . sStacks) json

  whoImportsThisValue e = do
    json <- eitherDecode <$> jsonForListImports e :: IO (Either String Imports)
    either (const (pure [])) (pure . iStackNames) json

buildDependencyGraph :: AWSExecution m => StackName -> m Dependency
buildDependencyGraph name | trace ("\nbDG called " ++ show name ++ "\n") False = undefined
buildDependencyGraph name = do
    outputs <- findExportsByStack name
    importers <- mapM whoImportsThisValue outputs
    let children = sort $ nub $ concat importers
    downstreamDeps <- mapM buildDependencyGraph children
    pure $ Map.unionsWith (\new old->nub ((++) new old))
                          (downstreamDeps ++ [Map.singleton name children])
