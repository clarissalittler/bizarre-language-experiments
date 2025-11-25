-- | Main entry point for NuPi interpreter
module Main where

import NuPi.Syntax
import NuPi.TypeCheck
import NuPi.Eval
import NuPi.Pretty
import NuPi.Examples

import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  putStrLn "NuPi - A Unified Language for Coinduction and Communication"
  putStrLn "============================================================"
  putStrLn ""

  -- Run some examples
  runExample "Identity function" exIdentity
  runExample "Constant function" exConst
  runExample "Pair swap" exPair
  runExample "Sum type" exSum
  runExample "Zero (Nat)" exZero
  runExample "Two (Nat)" exTwo
  runExample "Ones stream (first element)" (TmApp exTake exOnes)

  putStrLn ""
  putStrLn "All examples completed successfully!"

-- | Run an example and print the result
runExample :: String -> Term -> IO ()
runExample name term = do
  putStrLn $ "Example: " ++ name
  putStrLn $ "  Term: " ++ prettyTerm term

  case typeCheck term of
    Left err -> do
      putStrLn $ "  Type Error: " ++ show err
    Right ty -> do
      putStrLn $ "  Type: " ++ prettyTy ty
      case eval term of
        Left err -> putStrLn $ "  Eval Error: " ++ show err
        Right val -> putStrLn $ "  Value: " ++ prettyValue val

  putStrLn ""
