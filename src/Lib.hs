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
-- Vector is the type Aeson uses to represent JSON arrays
import qualified Data.Vector as V
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

import Data.Aeson.Types    -- that's where Parser comes from

someFunc :: IO ()
someFunc = do
  tree <- buildTree (StackName "kubernetes-staging-controller")
  putStrLn $ show tree

data Dependency = Dependency {name :: StackName,
                              deps :: [Dependency]} deriving (Show)

newtype StackName  = StackName  String deriving (Show, Generic, Eq)
newtype StackId    = StackId    String deriving (Show, Generic, Eq)
newtype ExportName = ExportName String deriving (Show, Generic)

findExportsJSON :: StackName -> IO String
findExportsJSON (StackName s) = do
  (code, stdout, stderr) <- readProcessWithExitCode "aws" ["cloudformation", "describe-stacks", "--stack-name", s] ""
  case code of
    ExitSuccess -> return stdout
    _ -> error (show stderr)

data Stack = Stack {sStackId :: StackId,
                    sStackName :: StackName,
                    sExports :: [Export]} deriving Show

data StacksResult = StacksResult [Stack] deriving Show
data Export = Export ExportName deriving Show

dependents :: ExportName -> IO String
dependents (ExportName e) = do
  (code, stdout, stderr) <- readProcessWithExitCode "aws" ["cloudformation", "list-imports", "--export-name", e] ""
  case code of
    ExitSuccess -> return stdout
    _ -> return ""
    -- _ -> error (show stderr)

data Imports = Imports [StackName]
  deriving (Show, Generic)

instance FromJSON Export where
  parseJSON = withObject "output object" $ \t -> do
    exportName <- t .: "ExportName"
    return (Export exportName)

instance FromJSON StacksResult where
  parseJSON = withObject "stacks result object" $ \t -> do
    stacksArray <- t .: "Stacks"
    return (StacksResult stacksArray)

instance FromJSON Stack where
  parseJSON = withObject "stack summary object" $ \t -> do
    stackid <- t .: "StackId"
    stackname <- t .: "StackName"
    outputs <- t .:? "Outputs" .!= []
    return (Stack stackid stackname outputs)

instance FromJSON Imports where
  parseJSON = withObject "imports result object" $ \t -> do
    list <- t .: "Imports"
    return (Imports list)

instance FromJSON StackName
instance FromJSON StackId
instance FromJSON ExportName

getExportsJSON :: StackName -> IO B.ByteString
getExportsJSON = fmap B.pack <$> findExportsJSON

getJSON :: ExportName -> IO B.ByteString
getJSON = fmap B.pack <$> dependents

findExports :: StackName -> IO [ExportName]
findExports s = do
  sr <- (eitherDecode <$> getExportsJSON s) :: IO (Either String StacksResult)
  case sr of
    Left err -> error err
    Right (StacksResult ps) -> pure $ map (\ (Export e) -> e) $ concatMap sExports ps

findImports :: ExportName -> IO Imports
findImports e = do
  d <- (eitherDecode <$> getJSON e) :: IO (Either String Imports)
  case d of
    Left err -> pure (Imports []) -- assume that empty aws feedback means nobody uses this Output
    Right ps -> pure ps

getImport :: Imports -> [StackName]
getImport (Imports i) = i

-- EXAMPLES

-- find the stacks that import this value:
tryit :: IO Imports
tryit = findImports (ExportName "kubernetes-staging-security-groups-ControllerInternalLBSg")

-- find the outputs of that stack:
outs :: IO [ExportName]
outs = do
  (Imports stacks) <- tryit
  let [name] = stacks
  findExports name

buildTree :: StackName -> IO Dependency
buildTree = buildTreeNoDup []

buildTreeNoDup :: [StackName] -> StackName -> IO Dependency
buildTreeNoDup alreadySeen name = do
  outputs <- findExports name -- :: [ExportName]
  imports <- mapM findImports outputs -- :: [Imports]
  let downstreams = nub $ filter (\sn -> not $ elem sn alreadySeen) $ concatMap getImport imports -- :: [StackName]
  downstreamDeps <- (mapM (\x -> buildTreeNoDup (alreadySeen++downstreams) x) downstreams)
  return $ Dependency name downstreamDeps


exampleDep2 = Dependency {name = StackName "kubernetes-staging-controller", deps = [Dependency {name = StackName "kubernetes-staging-controller-dns", deps = [Dependency {name = StackName "kubernetes-staging-worker", deps = [Dependency {name = StackName "kubernetes-staging-worker-dns", deps = [Dependency {name = StackName "samson-dns-staging", deps = []},Dependency {name = StackName "treasure-hunt-dynamic-df11859c-dns", deps = []},Dependency {name = StackName "treasure-hunt-staging-dns", deps = []}]}]}]},Dependency {name = StackName "fluentd-shiny-staging-iam", deps = []},Dependency {name = StackName "kubernetes-staging-logging-bucket", deps = []}]}

test :: Dependency -> Tree String
test (Dependency (StackName name) deps) = Node name (map test deps)

asdf = putStrLn $ drawTree (test exampleDep2)

postorder :: Tree String -> [String]
postorder(Node lab kids) = concatMap postorder kids ++ [lab]
