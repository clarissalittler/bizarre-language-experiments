{-# LANGUAGE OverloadedStrings #-}

-- | Example: Infinite Streams in NuPi
--
-- This file demonstrates coinductive types (streams) in NuPi
module Main where

import NuPi.Syntax
import NuPi.TypeCheck
import NuPi.Eval
import NuPi.Pretty

-- | Stream of integers: νX. Int × X
intStream :: Ty
intStream = TyNu "X" (TyProd TyInt (TyVar "X"))

-- | Create a constant stream: repeat n = n, n, n, ...
-- repeat : Int → Stream Int
-- repeat n = coiter[Stream Int](n, λs. (s, s))
repeatStream :: Term
repeatStream = TmLam "n" TyInt
  (TmCoiter intStream
    (TmVar "n")
    (TmLam "s" TyInt (TmPair (TmVar "s") (TmVar "s"))))

-- | Get the head of a stream
-- head : Stream Int → Int
streamHead :: Term
streamHead = TmLam "stream" intStream
  (TmFst (TmUnfoldNu (TmVar "stream")))

-- | Get the tail of a stream
-- tail : Stream Int → Stream Int
streamTail :: Term
streamTail = TmLam "stream" intStream
  (TmSnd (TmUnfoldNu (TmVar "stream")))

-- | Example: Create a stream of 42s and get the first element
example1 :: Term
example1 = TmApp streamHead (TmApp repeatStream (TmInt 42))

-- | Example: Get the head of the tail of a stream
example2 :: Term
example2 = TmApp streamHead (TmApp streamTail (TmApp repeatStream (TmInt 7)))

main :: IO ()
main = do
  putStrLn "NuPi Stream Examples"
  putStrLn "===================="
  putStrLn ""

  runExample "repeat function type" repeatStream
  runExample "head of repeat(42)" example1
  runExample "head(tail(repeat(7)))" example2

runExample :: String -> Term -> IO ()
runExample name term = do
  putStrLn $ "Example: " ++ name
  case typeCheck term of
    Left err -> putStrLn $ "  Type Error: " ++ show err
    Right ty -> do
      putStrLn $ "  Type: " ++ prettyTy ty
      case eval term of
        Left err -> putStrLn $ "  Eval Error: " ++ show err
        Right val -> putStrLn $ "  Value: " ++ prettyValue val
  putStrLn ""
