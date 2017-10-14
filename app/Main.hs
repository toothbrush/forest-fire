module Main where

import Lib

import Paths_forest_fire (version)
import Data.Version (showVersion)
import Console.Options
import System.IO
import System.Exit
import Data.Monoid ((<>), mappend)

main :: IO ()
main = defaultMain $ do
  programName "forest-fire"
  programDescription $ descr ++ usage
  programVersion version
  flagH    <- flag $ FlagShort 'h' <> FlagLong "help" <> FlagDescription "   Print this usage information and quit."
  flagV    <- flag $ FlagShort 'v' <> FlagLong "version" <> FlagDescription "Print version info and quit."
  flagA    <- flag $ FlagShort 'd' <> FlagLong "delete" <> FlagDescription " Actually perform the deletion (dry-run by default)."
  allArgs  <- remainingArguments "FILE"
  action $ \toParam -> do
      hSetBuffering stdout LineBuffering -- or even NoBuffering
      let showVersion  = toParam flagV
      let reallyDelete = toParam flagA
      let stackName    = toParam allArgs
      case showVersion of
        True  -> printVersion
        False -> case stackName of
                   [s] -> if reallyDelete
                          then actuallyDoTheDelete s
                          else showDeletionPlan s
                   _   -> do putStrLn "[ERROR] Please specify one stack name."
                             putStrLn usage
                             exitFailure

printVersion = do
  putStrLn $ "forest-fire v" ++ showVersion version

descr = "Recursively find and delete CFn dependencies!\n\n"
usage = "usage: forest-fire <stackname> [--delete]\n" ++
        "or:    forest-fire --help"
