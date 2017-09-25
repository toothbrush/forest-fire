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
          (Identity $ Dependency (StackName "a")
            [Dependency (StackName "b")
             [Dependency (StackName "c") []]])
          (buildDependencyGraph (StackName "a"))

instance AWSExecution Identity where
  findExportsByStack (StackName "a") = return [ExportName "a_exp"]
  findExportsByStack (StackName "b") = return [ExportName "b_exp"]
  findExportsByStack (StackName "c") = return []
  whoImportsThisValue (ExportName "a_exp") = return [StackName "b"]
  whoImportsThisValue (ExportName "b_exp") = return [StackName "c"]
