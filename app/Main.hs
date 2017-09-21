module Main where

import Lib
import Console.Options

main :: IO ()
main = defaultMain $ do
  programName "forest-fire"
  programDescription "Recursively delete CFn dependencies"
  flagA    <- flag $ FlagLong "delete"
  allArgs  <- remainingArguments "FILE"
  action $ \toParam -> do
      let reallyDelete = toParam flagA
      let stackName    = toParam allArgs
      case stackName of
        [s] -> if reallyDelete
               then actuallyDoTheDelete s
               else showDeletionPlan s
        _   -> printUsage

printUsage = do
  putStrLn "\nforest-fire"
  putStrLn "-----------\n"
  putStrLn "Usage:\n"
  putStrLn "  forest-fire <stackname> [--delete]\n"
  putStrLn "Flags:\n"
  putStrLn "--delete       To actually perform the deletion."
  putStrLn ""
