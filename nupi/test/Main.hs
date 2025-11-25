-- | Test suite for NuPi
module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import qualified TypeCheckTests
import qualified EvalTests
import qualified IntegrationTests

main :: IO ()
main = do
  putStrLn "Running NuPi test suite..."
  putStrLn ""

  counts <- runTestTT $ TestList
    [ TestLabel "TypeCheck" TypeCheckTests.tests
    , TestLabel "Eval" EvalTests.tests
    , TestLabel "Integration" IntegrationTests.tests
    ]

  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
