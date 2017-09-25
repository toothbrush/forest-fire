{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Control.Monad.Identity

import Types
import Lib

tests =
  testGroup
    "Unit tests"
    [trivialDepsTree]

main = defaultMain tests

trivialDepsTree =
  testCase "simple deletion case" $
      assertEqual []
          (Dependency (StackName "a")
            [Dependency (StackName "b")
             [Dependency (StackName "c") []]])
          (runIdentity $ unTest1 $ buildDependencyGraph1 (StackName "a"))
  where buildDependencyGraph1 = buildDependencyGraph :: StackName -> Test1 Dependency

newtype Test1 a = Test1 { unTest1 :: Identity a }
  deriving (Monad,Functor,Applicative)

instance AWSExecution Test1 where
  findExportsByStack (StackName "a") = return [ExportName "a_exp"]
  findExportsByStack (StackName "b") = return [ExportName "b_exp"]
  findExportsByStack (StackName "c") = return []
  whoImportsThisValue (ExportName "a_exp") = return [StackName "b"]
  whoImportsThisValue (ExportName "b_exp") = return [StackName "c"]
