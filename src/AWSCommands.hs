module AWSCommands where

import Types
import qualified Data.ByteString.Lazy.Char8 as B
import System.Process
import GHC.IO.Exception

executeAWScommand :: Bool -> [String] -> IO B.ByteString
executeAWScommand allowFail args = do
  putStrLn $ "[EXEC] aws " ++ unwords args
  (code, stdout, stderr) <- readProcessWithExitCode "aws" args ""
  case code of
    ExitSuccess -> pure (B.pack stdout)
    _ -> if allowFail
         then do putStrLn "[FAIL] Command failed, continuing."
                 pure (B.pack "")
         else do putStrLn "[FAIL] Aborting."
                 putStrLn stderr
                 error $ show code

jsonForDescribeStacks :: StackName -> IO B.ByteString
jsonForDescribeStacks (StackName s) =
  executeAWScommand True [ "cloudformation"
                         , "describe-stacks"
                         , "--stack-name", s]

jsonForListImports :: ExportName -> IO B.ByteString
jsonForListImports (ExportName e) =
  -- Don't fail: if an export isn't imported by anything, aws-cli
  -- returns an error.
  executeAWScommand True [ "cloudformation"
                         , "list-imports"
                         , "--export-name", e]

doDeletionWait :: StackName -> IO ()
doDeletionWait (StackName s) = do
  putStrLn $ "Issuing delete command on stack " ++ s ++ "."
  _ <- executeAWScommand False [ "cloudformation"
                               , "delete-stack"
                               , "--stack-name", s]
  putStrLn "Awaiting completion... (60 minute timeout)"
  _ <- executeAWScommand False [ "cloudformation"
                               , "wait"
                               , "stack-delete-complete"
                               , "--no-paginate"
                               , "--stack-name", s]
  putStrLn $ "Removal of " ++ s ++ " complete."
