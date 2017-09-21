{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.Tree
import Data.Tree.Pretty
import Data.Aeson
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics
import System.Process
import Data.Maybe
import GHC.IO.Exception

import Data.Aeson.Types    -- that's where Parser comes from

someFunc :: IO ()
someFunc = do
  -- let candidate = StackName "kubernetes-dynamic-91acf0ef-lifecycle"
  let candidate = StackName "kubernetes-dynamic-91acf0ef-controller-dns"
  putStrLn $ "Retrieving dependencies of " ++ show candidate ++ "..."
  tree <- buildTree candidate
  -- let tree = exampleDep2
  putStrLn "Done.  Delete these stacks in postorder traversal:\n"
  putStrLn $ drawTree (dependencyToTree tree)
  putStrLn "This would be postorder:\n"
  mapM_ putStrLn $ postorder (dependencyToTree tree)
  -- res <- mapM (executeEcho . singleton) $ postorder (dependencyToTree tree)
  -- mapM_ putStrLn res

tryDelete :: IO ()
tryDelete = do
  let candidate = StackName "kubernetes-dynamic-91acf0ef-controller-dns"
  putStrLn $ "Retrieving dependencies of " ++ show candidate ++ "..."
  tree <- buildTree candidate
  -- let tree = exampleDep2
  -- putStrLn "Done.  Delete these stacks in postorder traversal:\n"
  -- putStrLn $ drawTree (dependencyToTree tree)
  putStrLn "Deleting..."
  -- mapM_ putStrLn $ postorder (dependencyToTree tree)
  mapM_ (doDeletionWait . StackName) $ postorder (dependencyToTree tree)
  -- mapM_ putStrLn res

-- singleton = pure -- is actually pure for List Monad

data Dependency = Dependency {name :: StackName,
                              deps :: [Dependency]} deriving (Show)
data Stack = Stack {sStackId :: StackId,
                    sStackName :: StackName,
                    sExports :: [Export]} deriving Show

newtype StacksResult = StacksResult [Stack] deriving Show
newtype Export       = Export ExportName deriving Show

newtype StackName  = StackName  String deriving (Show, Generic, Eq)
newtype StackId    = StackId    String deriving (Show, Generic, Eq)
newtype ExportName = ExportName String deriving (Show, Generic)
newtype Imports    = Imports    [StackName] deriving (Show)

executeEcho :: [String] -> IO String
executeEcho args = do
  (code, stdout, stderr) <- readProcessWithExitCode "echo" args ""
  case code of
    ExitSuccess -> pure stdout
    _ -> error (show stderr)

doDeletionWait :: StackName -> IO ()
doDeletionWait (StackName s) = do
  putStrLn $ "Deleting " ++ s ++ "."
  res <- executeAWScommand False ["cloudformation", "delete-stack", "--stack-name", s]
  putStrLn "Awaiting completion... (60 minute timeout)"
  res <- executeAWScommand False ["cloudformation", "wait", "stack-delete-complete", "--no-paginate", "--stack-name", s]
  putStrLn $ "Delete of " ++ s ++ " complete."

executeAWScommand :: Bool -> [String] -> IO B.ByteString
executeAWScommand allowFail args = do
  (code, stdout, stderr) <- readProcessWithExitCode "aws" args ""
  case code of
    ExitSuccess -> pure (B.pack stdout)
    _ -> if allowFail
         then pure (B.pack "")
         else error (show stderr)

jsonForDescribeStacks :: StackName -> IO B.ByteString
jsonForDescribeStacks (StackName s) =
  executeAWScommand False ["cloudformation", "describe-stacks", "--stack-name", s]

jsonForListImports :: ExportName -> IO B.ByteString
jsonForListImports (ExportName e) =
  -- this one's optional, don't fail.
  executeAWScommand True ["cloudformation", "list-imports", "--export-name", e]

instance FromJSON Export where
  parseJSON = withObject "output object" $ \t -> do
    exportName <- t .: "ExportName"
    pure (Export exportName)

instance FromJSON StacksResult where
  parseJSON = withObject "stacks result object" $ \t -> do
    stacksArray <- t .: "Stacks"
    pure (StacksResult stacksArray)

instance FromJSON Stack where
  parseJSON = withObject "stack summary object" $ \t -> do
    stackid <- t .: "StackId"
    stackname <- t .: "StackName"
    outputs <- t .:? "Outputs" .!= [] -- If there are no outputs, return []
    pure (Stack stackid stackname outputs)

instance FromJSON Imports where
  parseJSON = withObject "imports result object" $ \t -> do
    list <- t .: "Imports"
    pure (Imports list)

instance FromJSON StackName
instance FromJSON StackId
instance FromJSON ExportName

findExports :: StackName -> IO [ExportName]
findExports = generalise
     jsonForDescribeStacks
     error
     (\ (StacksResult ps) -> pure $ map (\ (Export e) -> e) $ concatMap sExports ps)

findImports :: ExportName -> IO Imports
findImports = generalise
    jsonForListImports
    (\ _ -> pure (Imports []))
    pure

-- findExports :: StackName -> IO [ExportName]
generalise whence leftThing rightThing s = do
  sr <- eitherDecode <$> whence s --- :: IO (Either String StacksResult)
  case sr of
    Left err -> leftThing err
    Right ps -> rightThing ps

getImport :: Imports -> [StackName]
getImport (Imports i) = i

buildTree :: StackName -> IO Dependency
buildTree = buildTree' []

buildTree' :: [StackName] -> StackName -> IO Dependency
buildTree' alreadySeen name = do
  outputs <- findExports name
  imports <- mapM findImports outputs
  let downstreams = nub $ filter (`notElem` alreadySeen) $ concatMap getImport imports
  downstreamDeps <- mapM (buildTree' (alreadySeen ++ downstreams)) downstreams
  pure $ Dependency name downstreamDeps


exampleDep2 = Dependency {name = StackName "kubernetes-staging-controller", deps = [Dependency {name = StackName "kubernetes-staging-controller-dns", deps = [Dependency {name = StackName "kubernetes-staging-worker", deps = [Dependency {name = StackName "kubernetes-staging-worker-dns", deps = [Dependency {name = StackName "samson-dns-staging", deps = []},Dependency {name = StackName "treasure-hunt-dynamic-df11859c-dns", deps = []},Dependency {name = StackName "treasure-hunt-staging-dns", deps = []}]}]}]},Dependency {name = StackName "fluentd-shiny-staging-iam", deps = []},Dependency {name = StackName "kubernetes-staging-logging-bucket", deps = []}]}

dependencyToTree :: Dependency -> Tree String
dependencyToTree (Dependency (StackName name) deps) = Node name (map dependencyToTree deps)

asdf = putStrLn $ drawTree (dependencyToTree exampleDep2)

postorder :: Tree a -> [a]
postorder (Node label kids) = concatMap postorder kids ++ [label]
