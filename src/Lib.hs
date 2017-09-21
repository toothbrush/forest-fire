{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO
-- * test suite (around JSON parsing, mostly)
-- * modularise once boundaries are clear
-- * clean up code a bit...
-- * readme (w/ how to install and use)
-- * send to Hackage/Stackage

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

showDeletionPlan :: String -> IO ()
showDeletionPlan stackName = do
  putStrLn $ "Retrieving dependencies of " ++ stackName ++ "..."
  tree <- buildTree (StackName stackName)
  -- let tree = exampleDep2
  putStrLn "Done.  Delete these stacks in postorder traversal:\n"
  putStrLn $ drawTree (dependencyToTree tree)
  putStrLn "Or, delete manually in this order:\n"
  mapM_ putStrLn $ postorder (dependencyToTree tree)

actuallyDoTheDelete :: String -> IO ()
actuallyDoTheDelete stackName = do
  putStrLn $ "Retrieving dependencies of " ++ stackName ++ "..."
  tree <- buildTree (StackName stackName)
  putStrLn "Deleting dependencies and stack..."
  mapM_ (doDeletionWait . StackName) $ postorder (dependencyToTree tree)

data Dependency = Dependency {dStackName    :: StackName,
                              dDependencies :: [Dependency]} deriving (Show)

data Stack = Stack {sStackId   :: StackId,
                    sStackName :: StackName,
                    sExports   :: [Export]} deriving Show

newtype StacksResult = StacksResult { srStacks :: [Stack]} deriving Show
newtype Export       = Export       { eName :: ExportName} deriving Show

newtype StackName  = StackName  String      deriving (Show, Generic, Eq)
newtype StackId    = StackId    String      deriving (Show, Generic, Eq)
newtype ExportName = ExportName String      deriving (Show, Generic)
newtype Imports    = Imports    { iStackNames :: [StackName] } deriving (Show)

executeEcho :: [String] -> IO String
executeEcho args = do
  (code, stdout, stderr) <- readProcessWithExitCode "echo" args ""
  case code of
    ExitSuccess -> pure stdout
    _ -> error (show stderr)

doDeletionWait :: StackName -> IO ()
doDeletionWait (StackName s) = do
  putStrLn $ "Issuing delete command on stack " ++ s ++ "."
  res <- executeAWScommand False ["cloudformation", "delete-stack", "--stack-name", s]
  putStrLn "Awaiting completion... (60 minute timeout)"
  res <- executeAWScommand False ["cloudformation", "wait", "stack-delete-complete", "--no-paginate", "--stack-name", s]
  putStrLn $ "Removal of " ++ s ++ " complete."

executeAWScommand :: Bool -> [String] -> IO B.ByteString
executeAWScommand allowFail args = do
  putStrLn $ "[EXEC] aws " ++ unwords args
  (code, stdout, stderr) <- readProcessWithExitCode "aws" args ""
  case code of
    ExitSuccess -> pure (B.pack stdout)
    _ -> if allowFail
         then pure (B.pack "")
         else do putStrLn stderr
                 error $ show code

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
     (pure . map eName . concatMap sExports . srStacks)

findImports :: ExportName -> IO Imports
findImports = generalise
    jsonForListImports
    (const $ pure (Imports []))
    pure

-- findExports :: StackName -> IO [ExportName]
generalise whence leftThing rightThing s = do
  sr <- eitherDecode <$> whence s --- :: IO (Either String StacksResult)
  case sr of
    Left err -> leftThing err
    Right ps -> rightThing ps

buildTree :: StackName -> IO Dependency
buildTree = buildTree' []

buildTree' :: [StackName] -> StackName -> IO Dependency
buildTree' alreadySeen name = do
  outputs <- findExports name
  imports <- mapM findImports outputs
  let downstreams = nub $ filter (`notElem` alreadySeen) $ concatMap iStackNames imports
  downstreamDeps <- mapM (buildTree' (alreadySeen ++ downstreams)) downstreams
  pure $ Dependency name downstreamDeps


exampleDep2 = Dependency {dStackName = StackName "kubernetes-staging-controller", dDependencies = [Dependency {dStackName = StackName "kubernetes-staging-controller-dns", dDependencies = [Dependency {dStackName = StackName "kubernetes-staging-worker", dDependencies = [Dependency {dStackName = StackName "kubernetes-staging-worker-dns", dDependencies = [Dependency {dStackName = StackName "samson-dns-staging", dDependencies = []},Dependency {dStackName = StackName "treasure-hunt-dynamic-df11859c-dns", dDependencies = []},Dependency {dStackName = StackName "treasure-hunt-staging-dns", dDependencies = []}]}]}]},Dependency {dStackName = StackName "fluentd-shiny-staging-iam", dDependencies = []},Dependency {dStackName = StackName "kubernetes-staging-logging-bucket", dDependencies = []}]}

dependencyToTree :: Dependency -> Tree String
dependencyToTree (Dependency (StackName name) deps) = Node name (map dependencyToTree deps)

postorder :: Tree a -> [a]
postorder (Node label kids) = concatMap postorder kids ++ [label]
