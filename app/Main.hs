module Main where

import Lib

import Paths_forest_fire (version)
import Data.Version (showVersion)
import Console.Options
import System.IO
import System.Exit
import Data.Monoid ((<>), mappend)
import Data.List (intercalate)

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
      if showVersion
        then printVersion
        else (case stackName of
                xs@(_:_) -> if reallyDelete
                            then doDeletes xs
                            else showDeletes xs
                []  -> do putStrLn "[ERROR] Please specify at least one stack name."
                          putStrLn usage
                          exitFailure)

doDeletes :: [String] -> IO ()
doDeletes = mapM_ actuallyDoTheDelete

showDeletes :: [String] -> IO ()
showDeletes xs = do mapM_ outputDeletionPlan xs
                    putStrLn "\nIf you trust this app you can execute:"
                    putStrLn $ "forest-fire " ++ unwords xs ++ " --delete\n"

printVersion = putStrLn $ "forest-fire version " ++ showVersion version

descr = "Recursively find and delete CFn dependencies!\n\n"
usage = "usage: forest-fire [--delete] <stacknames>+\n" ++
        "or:    forest-fire --help"
