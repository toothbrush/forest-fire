{-# LANGUAGE OverloadedStrings #-}

module JSONInstances where

import Types
import AWSCommands

import Data.Aeson
import Data.Aeson.Types    -- that's where Parser comes from

instance FromJSON Export where
  parseJSON = withObject "Export" $ \t ->
    Export <$> t .:? "ExportName" .!= ExportName ""

instance FromJSON Stacks where
  parseJSON = withObject "Stacks" $ \t ->
    Stacks <$> t .: "Stacks"

instance FromJSON Stack where
  parseJSON = withObject "Stack" $ \t ->
    Stack <$> t .:  "StackId"
          <*> t .:  "StackName"
          <*> t .:? "Outputs" .!= [] -- If there are no outputs, return []

instance FromJSON Imports where
  parseJSON = withObject "Imports" $ \t ->
    Imports <$> t .: "Imports"

instance FromJSON StackName
instance FromJSON StackId
instance FromJSON ExportName

class Monad m => AWSExecution m where
  findExportsByStack :: StackName -> m [ExportName]
  whoImportsThisValue :: ExportName -> m [StackName]

instance AWSExecution IO where
  findExportsByStack s = do
    json <- eitherDecode <$> jsonForDescribeStacks s :: IO (Either String Stacks)
    either (const (pure [])) (pure . map eName . filter anon . concatMap sExports . sStacks) json
      where anon (Export name) | name == ExportName "" = False
                               | otherwise             = True

  whoImportsThisValue e = do
    json <- eitherDecode <$> jsonForListImports e :: IO (Either String Imports)
    either (const (pure [])) (pure . iStackNames) json
