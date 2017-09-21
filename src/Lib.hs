-- TODO
-- * test suite (around JSON parsing, mostly)
-- * readme (w/ how to install and use)
-- * send to Hackage/Stackage

module Lib where

import Types
import JSONInstances
import AWSCommands
import TreeUtils

import Data.Tree
import Data.Tree.Pretty
import Data.List
import Data.Maybe
import Data.Aeson

showDeletionPlan :: String -> IO ()
showDeletionPlan stackName = do
  putStrLn $ "Retrieving dependencies of " ++ stackName ++ "..."
  tree <- buildDependencyGraph (StackName stackName)
  -- let tree = exampleDep2
  putStrLn "Done.  Delete these stacks in postorder traversal:\n"
  putStrLn $ drawTree (dependencyToTree tree)
  putStrLn "Or, delete manually in this order:\n"
  mapM_ putStrLn $ postorder (dependencyToTree tree)

actuallyDoTheDelete :: String -> IO ()
actuallyDoTheDelete stackName = do
  putStrLn $ "Retrieving dependencies of " ++ stackName ++ "..."
  tree <- buildDependencyGraph (StackName stackName)
  putStrLn "Deleting dependencies and stack..."
  mapM_ (doDeletionWait . StackName) $ postorder (dependencyToTree tree)

findExportsByStack :: StackName -> IO [ExportName]
findExportsByStack s = do
  json <- eitherDecode <$> jsonForDescribeStacks s
  either error (pure . map eName . concatMap sExports . sStacks) json

whoImportsThisValue :: ExportName -> IO [StackName]
whoImportsThisValue e = do
  json <- eitherDecode <$> jsonForListImports e :: IO (Either String Imports)
  either (const (pure [])) (pure . iStackNames) json

buildDependencyGraph :: StackName -> IO Dependency
buildDependencyGraph = buildDependencyGraph' [] -- the empty list is because we haven't yet queried any stacks
  where buildDependencyGraph' :: [StackName] -> StackName -> IO Dependency
        buildDependencyGraph' alreadySeen name = do
          outputs <- findExportsByStack name
          importers <- mapM whoImportsThisValue outputs
          let downstreams = nub $ filter (`notElem` alreadySeen) $ concat importers
          downstreamDeps <- mapM (buildDependencyGraph' (alreadySeen ++ downstreams)) downstreams
          pure $ Dependency name downstreamDeps

