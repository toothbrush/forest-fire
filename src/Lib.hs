module Lib where

import Types
import JSONInstances
import AWSCommands

import Data.List (nub, sort, delete)
import Data.Map (Map)
import qualified Data.Map as Map

outputDeletionPlan :: String -> IO Dependency
outputDeletionPlan stackName = do
  putStrLn $ "Retrieving dependencies of " ++ stackName ++ "..."
  dag <- buildDependencyGraph (StackName stackName)
  putStrLn "Done.  Deletion order:\n"
  mapM_ (putStrLn . (++) "  " . show) $ deletionOrder dag
  return dag

actuallyDoTheDelete :: String -> IO ()
actuallyDoTheDelete stackName = do
  dag <- outputDeletionPlan stackName
  putStrLn "Deleting dependencies and stack..."
  mapM_ doDeletionWait $ deletionOrder dag

deletionOrder :: Dependency -> [StackName]
deletionOrder d | d == Map.empty = []
deletionOrder d | null (safe d) = error "bork bork circular dependencies!"
deletionOrder d = safe d ++ deletionOrder (without (safe d) d)
  where
    without :: [StackName] -> Dependency -> Dependency
    without [] d = d
    without (stack : ss) d = Map.map (delete stack) $ Map.delete stack (without ss d)

safe :: Dependency -> [StackName]
safe d = Map.keys $ Map.filter null d

buildDependencyGraph :: AWSExecution m => StackName -> m Dependency
buildDependencyGraph name = do
    outputs <- findExportsByStack name
    importers <- mapM whoImportsThisValue outputs
    let children = sort $ nub $ concat importers
    downstreamDeps <- mapM buildDependencyGraph children
    pure $ Map.unionsWith (\new old -> nub $ (++) new old)
                          (downstreamDeps ++ [Map.singleton name children])
