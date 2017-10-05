{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Control.Monad.Identity
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map

import Types
import Lib
import JSONInstances

tests =
  testGroup
    "Unit tests"
    [trivialDepsTree, dagIsntTree
    ,deletionTrivial, deletionDAG
    ,testJSONparsing]

main = defaultMain tests

trivialDepsTree =
  testCase "simple deletion case" $
      assertEqual []
          (Map.fromList [(StackName "a", [StackName "b"])
                        ,(StackName "b", [StackName "c"])
                        ,(StackName "c", [])
                        ])
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

dagIsntTree =
  testCase "pathological DAG case" $
      assertEqual []
          (Map.fromList [ (StackName "a", [StackName "c", StackName "d"])
                        , (StackName "d", [StackName "c"])
                        , (StackName "c", [])
                        ])
          (runIdentity $ unTest2 $ buildDependencyGraph1 (StackName "a"))
  where buildDependencyGraph1 = buildDependencyGraph :: StackName -> Test2 Dependency

newtype Test2 a = Test2 { unTest2 :: Identity a }
  deriving (Monad,Functor,Applicative)

instance AWSExecution Test2 where
  findExportsByStack (StackName "a") = return [ExportName "a_exp"]
  findExportsByStack (StackName "d") = return [ExportName "d_exp"]
  findExportsByStack (StackName "c") = return []
  whoImportsThisValue (ExportName "a_exp") = return [StackName "d", StackName "c"]
  whoImportsThisValue (ExportName "d_exp") = return [StackName "c"]

deletionTrivial =
  testCase "simple deletion order from test 1" $
      assertEqual []
          [StackName "c", StackName "b", StackName "a"]
          (deletionOrder (runIdentity $ unTest1 $ buildDependencyGraph1 (StackName "a")))
  where buildDependencyGraph1 = buildDependencyGraph :: StackName -> Test1 Dependency

deletionDAG =
  testCase "tricky deletion order from test 2" $
      assertEqual []
          [StackName "c", StackName "d", StackName "a"]
          (deletionOrder (runIdentity $ unTest2 $ buildDependencyGraph2 (StackName "a")))
  where buildDependencyGraph2 = buildDependencyGraph :: StackName -> Test2 Dependency


testJSONparsing =
  testCase "test parsing AWS output" $ do
    out <- stuff
    assertEqual []
      [ExportName "kubernetes-staging-controller-ELBControllerInternalHostedZone"
      ,ExportName "kubernetes-staging-controller-IAMRole"]
      out
    where stuff = do json <- eitherDecode <$> B.readFile "test/aws-output-test.json"
                     either error (pure . map eName . concatMap sExports . sStacks) json
