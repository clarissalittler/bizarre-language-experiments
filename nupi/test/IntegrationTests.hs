-- | Integration tests for NuPi
--
-- These tests verify end-to-end behavior including type checking + evaluation
module IntegrationTests (tests) where

import Test.HUnit
import NuPi.Syntax
import NuPi.TypeCheck
import NuPi.Eval
import NuPi.Types
import NuPi.Examples

-- | All integration tests
tests :: Test
tests = TestList
  [ testDuality
  , testTypePreservation
  , testProcessExecution
  , testComplexPrograms
  ]

-- | Test the duality operation
testDuality :: Test
testDuality = TestLabel "Duality" $ TestList
  [ "tensor to lollipop" ~:
      dual (TyTensor TyInt TyBool) ~?= TyLollipop TyInt TyBool

  , "lollipop to tensor" ~:
      dual (TyLollipop TyInt TyBool) ~?= TyTensor TyInt TyBool

  , "plus to with" ~:
      dual (TyPlus [("A", TyInt)]) ~?= TyWith [("A", TyInt)]

  , "with to plus" ~:
      dual (TyWith [("A", TyInt)]) ~?= TyPlus [("A", TyInt)]

  , "mu to nu" ~:
      dual (TyMu "X" (TyVar "X")) ~?= TyNu "X" (TyVar "X")

  , "nu to mu" ~:
      dual (TyNu "X" (TyVar "X")) ~?= TyMu "X" (TyVar "X")

  , "double dual is identity (simple)" ~:
      let ty = TyTensor TyInt TyBool
      in dual (dual ty) ~?= ty

  , "double dual with channel" ~:
      let ty = TyChan (TyTensor TyInt (TyLollipop TyBool TyUnit))
      in dual (dual ty) ~?= ty

  , "counter protocol dual" ~:
      -- Counter = νX. &{ Inc: X, Read: Int ⊗ X, Stop: 1 }
      -- dual(Counter) = μX. ⊕{ Inc: X, Read: Int ⊸ X, Stop: 1 }
      let counter = tyCounter
          dualCounter = dual counter
      in case dualCounter of
           TyMu _ (TyPlus alts) -> length alts == 3
           _ -> False
      ~?= True
  ]

-- | Test type preservation: well-typed terms remain well-typed through evaluation
testTypePreservation :: Test
testTypePreservation = TestLabel "Type Preservation" $ TestList
  [ "identity preserves type" ~:
      testPreservation exIdentity

  , "pair preserves type" ~:
      testPreservation exPair

  , "sum preserves type" ~:
      testPreservation exSum

  , "zero preserves type" ~:
      testPreservation exZero

  , "two preserves type" ~:
      testPreservation exTwo
  ]
  where
    -- A simple check: if term type-checks, it should evaluate without error
    testPreservation term =
      case typeCheck term of
        Left _ -> False
        Right _ -> case eval term of
          Left _ -> False
          Right _ -> True
      ~?= True

-- | Test process execution
testProcessExecution :: Test
testProcessExecution = TestLabel "Process Execution" $ TestList
  [ "return value" ~:
      runProcess (TmReturn (TmInt 42)) ~?= Right (VInt 42)

  , "return unit" ~:
      runProcess (TmReturn TmUnit) ~?= Right VUnit

  , "simple bind" ~:
      let term = TmBind "x" (TmReturn (TmInt 10))
                   (TmReturn (TmVar "x"))
      in runProcess term ~?= Right (VInt 10)
  ]

-- | Test complex programs
testComplexPrograms :: Test
testComplexPrograms = TestLabel "Complex Programs" $ TestList
  [ "church numerals" ~:
      -- Church encoding of 2: λf. λx. f (f x)
      let two = TmLam "f" (TyArrow TyInt TyInt)
                  (TmLam "x" TyInt
                    (TmApp (TmVar "f") (TmApp (TmVar "f") (TmVar "x"))))
          -- Successor function (would need arithmetic, so just identity)
          succ_ = TmLam "n" TyInt (TmVar "n")
          -- Apply two to succ and 0
          term = TmApp (TmApp two succ_) (TmInt 0)
      in case eval term of
           Right (VInt 0) -> True  -- Identity applied twice to 0 is 0
           _ -> False
      ~?= True

  , "nested let bindings" ~:
      let term = TmLet "a" (TmInt 1)
                   (TmLet "b" (TmInt 2)
                     (TmLet "c" (TmInt 3)
                       (TmPair (TmVar "a") (TmPair (TmVar "b") (TmVar "c")))))
      in eval term ~?= Right (VPair (VInt 1) (VPair (VInt 2) (VInt 3)))

  , "higher order function" ~:
      let apply = TmLam "f" (TyArrow TyInt TyInt)
                    (TmLam "x" TyInt (TmApp (TmVar "f") (TmVar "x")))
          id_ = TmLam "n" TyInt (TmVar "n")
          term = TmApp (TmApp apply id_) (TmInt 100)
      in eval term ~?= Right (VInt 100)

  , "stream head" ~:
      -- Get the head of the ones stream
      let term = TmFst (TmUnfoldNu exOnes)
      in eval term ~?= Right (VInt 1)

  , "nat case analysis" ~:
      -- Pattern match on zero
      let term = TmCase (TmUnfold exZero)
                   [ ("Zero", "_", TmInt 0)
                   , ("Succ", "n", TmInt 1)
                   ]
      in eval term ~?= Right (VInt 0)

  , "nat succ case analysis" ~:
      -- Pattern match on Succ Zero
      let term = TmCase (TmUnfold (TmApp exSucc exZero))
                   [ ("Zero", "_", TmInt 0)
                   , ("Succ", "n", TmInt 1)
                   ]
      in eval term ~?= Right (VInt 1)
  ]
