{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics

data Dependency = Dependency {dStackName    :: StackName,
                              dDependencies :: [Dependency]} deriving (Show)

data Stack = Stack {sStackId   :: StackId,
                    sStackName :: StackName,
                    sExports   :: [Export]} deriving Show

newtype Stacks = Stacks { sStacks :: [Stack]} deriving Show
newtype Export = Export { eName :: ExportName} deriving Show

newtype StackName  = StackName  String      deriving (Show, Generic, Eq)
newtype StackId    = StackId    String      deriving (Show, Generic, Eq)
newtype ExportName = ExportName String      deriving (Show, Generic)
newtype Imports    = Imports    { iStackNames :: [StackName] } deriving (Show)
