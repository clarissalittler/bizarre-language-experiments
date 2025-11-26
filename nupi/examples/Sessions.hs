{-# LANGUAGE OverloadedStrings #-}

-- | Example: Session-Typed Communication in NuPi
--
-- This file demonstrates session types and process communication
module Main where

import NuPi.Syntax
import NuPi.TypeCheck
import NuPi.Eval
import NuPi.Pretty

-- | A simple request-response protocol
-- RequestResponse = Int ⊗ (Int ⊸ 1)
-- The client sends an Int, then receives an Int, then closes
requestResponseTy :: Ty
requestResponseTy = TyTensor TyInt (TyLollipop TyInt TyUnit)

-- | Dual: the server receives an Int, then sends an Int, then closes
serverTy :: Ty
serverTy = dual requestResponseTy  -- Int ⊸ (Int ⊗ 1)

-- | A calculator service protocol using choice
-- Calculator = νX. &{ Add: Int ⊸ Int ⊸ Int ⊗ X, Quit: 1 }
calculatorTy :: Ty
calculatorTy = TyNu "X" (TyWith
  [ ("Add", TyLollipop TyInt (TyLollipop TyInt (TyTensor TyInt (TyVar "X"))))
  , ("Quit", TyUnit)
  ])

-- | Example: Create channels and return unit
simpleChannel :: Term
simpleChannel = TmBind "chans" (TmNewChan TyInt)
  (TmReturn TmUnit)

-- | Example: Simple send and return
simpleSend :: Term
simpleSend = TmBind "chans" (TmNewChan (TyTensor TyInt TyUnit))
  (TmLetTensor "c1" "c2" (TmVar "chans")
    (TmBind "_" (TmSend (TmVar "c1") (TmInt 42))
      (TmReturn TmUnit)))

main :: IO ()
main = do
  putStrLn "NuPi Session Type Examples"
  putStrLn "=========================="
  putStrLn ""

  putStrLn "Session Types:"
  putStrLn $ "  RequestResponse: " ++ prettyTy requestResponseTy
  putStrLn $ "  Server (dual):   " ++ prettyTy serverTy
  putStrLn $ "  Calculator:      " ++ prettyTy calculatorTy
  putStrLn ""

  runExample "simple channel creation" simpleChannel

runExample :: String -> Term -> IO ()
runExample name term = do
  putStrLn $ "Example: " ++ name
  case typeCheck term of
    Left err -> putStrLn $ "  Type Error: " ++ show err
    Right ty -> do
      putStrLn $ "  Type: " ++ prettyTy ty
      case runProcess term of
        Left err -> putStrLn $ "  Run Error: " ++ show err
        Right val -> putStrLn $ "  Result: " ++ prettyValue val
  putStrLn ""
