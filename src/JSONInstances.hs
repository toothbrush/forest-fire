{-# LANGUAGE OverloadedStrings #-}

module JSONInstances where

import Types

import Data.Aeson
import Data.Aeson.Types    -- that's where Parser comes from

instance FromJSON Export where
  parseJSON = withObject "Export" $ \t ->
    Export <$> t .: "ExportName"

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