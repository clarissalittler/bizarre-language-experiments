-- | Evaluation tests for NuPi
module EvalTests (tests) where

import Test.HUnit
import NuPi.Syntax
import NuPi.Eval
import NuPi.Examples

-- | All evaluation tests
tests :: Test
tests = TestList
  [ testPureEval
  , testPairEval
  , testFunctionEval
  , testSumEval
  , testInductiveEval
  , testCoinductiveEval
  ]

-- | Test evaluation of pure terms
testPureEval :: Test
testPureEval = TestLabel "Pure Evaluation" $ TestList
  [ "unit" ~: eval TmUnit ~?= Right VUnit
  , "int" ~: eval (TmInt 42) ~?= Right (VInt 42)
  , "bool true" ~: eval (TmBool True) ~?= Right (VBool True)
  , "bool false" ~: eval (TmBool False) ~?= Right (VBool False)
  ]

-- | Test pair operations
testPairEval :: Test
testPairEval = TestLabel "Pair Evaluation" $ TestList
  [ "pair creation" ~:
      eval (TmPair (TmInt 1) (TmInt 2)) ~?= Right (VPair (VInt 1) (VInt 2))

  , "fst" ~:
      eval (TmFst (TmPair (TmInt 1) (TmInt 2))) ~?= Right (VInt 1)

  , "snd" ~:
      eval (TmSnd (TmPair (TmInt 1) (TmInt 2))) ~?= Right (VInt 2)

  , "pair swap" ~:
      let term = TmLet "p" (TmPair (TmInt 1) (TmInt 2))
                   (TmPair (TmSnd (TmVar "p")) (TmFst (TmVar "p")))
      in eval term ~?= Right (VPair (VInt 2) (VInt 1))

  , "tensor" ~:
      eval (TmTensor (TmInt 3) (TmInt 4)) ~?= Right (VTensor (VInt 3) (VInt 4))

  , "let tensor" ~:
      let term = TmLetTensor "x" "y" (TmTensor (TmInt 5) (TmInt 6))
                   (TmPair (TmVar "y") (TmVar "x"))
      in eval term ~?= Right (VPair (VInt 6) (VInt 5))
  ]

-- | Test function evaluation
testFunctionEval :: Test
testFunctionEval = TestLabel "Function Evaluation" $ TestList
  [ "identity application" ~:
      eval (TmApp exIdentity (TmInt 42)) ~?= Right (VInt 42)

  , "const application" ~:
      eval (TmApp (TmApp exConst (TmInt 1)) (TmBool False)) ~?= Right (VInt 1)

  , "nested lambda" ~:
      let term = TmApp (TmApp
                   (TmLam "x" TyInt (TmLam "y" TyInt (TmVar "x")))
                   (TmInt 10))
                   (TmInt 20)
      in eval term ~?= Right (VInt 10)

  , "linear lambda" ~:
      let term = TmApp (TmLinLam "x" TyInt (TmVar "x")) (TmInt 99)
      in eval term ~?= Right (VInt 99)
  ]

-- | Test sum type evaluation
testSumEval :: Test
testSumEval = TestLabel "Sum Evaluation" $ TestList
  [ "left injection" ~:
      let sumTy = TyPlus [("Left", TyInt), ("Right", TyBool)]
          term = TmCase (TmInj "Left" (TmInt 42) sumTy)
                   [ ("Left", "n", TmVar "n")
                   , ("Right", "b", TmInt 0)
                   ]
      in eval term ~?= Right (VInt 42)

  , "right injection" ~:
      let sumTy = TyPlus [("Left", TyInt), ("Right", TyBool)]
          term = TmCase (TmInj "Right" (TmBool True) sumTy)
                   [ ("Left", "n", TmVar "n")
                   , ("Right", "b", TmInt 0)
                   ]
      in eval term ~?= Right (VInt 0)

  , "record creation and selection" ~:
      let term = TmSelect "y" (TmRecord [("x", TmInt 1), ("y", TmInt 2)])
      in eval term ~?= Right (VInt 2)
  ]

-- | Test inductive type evaluation
testInductiveEval :: Test
testInductiveEval = TestLabel "Inductive Evaluation" $ TestList
  [ "zero" ~:
      case eval exZero of
        Right (VFold (VInj "Zero" VUnit)) -> True
        _ -> False
      ~?= True

  , "succ zero" ~:
      case eval (TmApp exSucc exZero) of
        Right (VFold (VInj "Succ" (VFold (VInj "Zero" VUnit)))) -> True
        _ -> False
      ~?= True

  , "unfold" ~:
      let term = TmUnfold (TmApp exSucc exZero)
      in case eval term of
           Right (VInj "Succ" _) -> True
           _ -> False
      ~?= True
  ]

-- | Test coinductive type evaluation
testCoinductiveEval :: Test
testCoinductiveEval = TestLabel "Coinductive Evaluation" $ TestList
  [ "ones stream creation" ~:
      case eval exOnes of
        Right (VCoiter VUnit _) -> True
        _ -> False
      ~?= True

  , "unfold ones stream" ~:
      let term = TmUnfoldNu exOnes
      in case eval term of
           Right (VPair (VInt 1) VUnit) -> True
           _ -> False
      ~?= True

  , "take from ones" ~:
      let term = TmApp exTake exOnes
      in eval term ~?= Right (VInt 1)

  , "nats stream" ~:
      let term = TmApp exNats (TmInt 5)
      in case eval term of
           Right (VCoiter (VInt 5) _) -> True
           _ -> False
      ~?= True
  ]
