{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Map (Map)

type Dependency = Map StackName [StackName] -- list of nodes with outgoing edge targets

data Stack = Stack {sStackId   :: StackId,
                    sStackName :: StackName,
                    sExports   :: [Export]} deriving Show

newtype Stacks = Stacks { sStacks :: [Stack]} deriving Show
newtype Export = Export { eName :: ExportName} deriving Show

newtype StackName  = StackName  String      deriving (Show, Generic, Eq, Ord)
newtype StackId    = StackId    String      deriving (Show, Generic, Eq)
newtype ExportName = ExportName String      deriving (Show, Generic, Eq)
newtype Imports    = Imports    { iStackNames :: [StackName] } deriving (Show)

-- Don't add this as a record field name above, it messes up the JSON
-- parser (specifically: makes it expect a record instead of just a
-- string).
snStackName (StackName s) = s
