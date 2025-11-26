-- | Type checking tests for NuPi
module TypeCheckTests (tests) where

import Test.HUnit
import NuPi.Syntax
import NuPi.TypeCheck
import NuPi.Examples

-- | All type checking tests
tests :: Test
tests = TestList
  [ testPureTerms
  , testLinearTerms
  , testInductiveTypes
  , testCoinductiveTypes
  , testSessionTypes
  , testLinearErrors
  ]

-- | Test pure (non-linear) terms
testPureTerms :: Test
testPureTerms = TestLabel "Pure Terms" $ TestList
  [ "unit" ~: typeCheck TmUnit ~?= Right TyUnit
  , "int" ~: typeCheck (TmInt 42) ~?= Right TyInt
  , "bool" ~: typeCheck (TmBool True) ~?= Right TyBool
  , "pair" ~: typeCheck (TmPair (TmInt 1) (TmBool True)) ~?= Right (TyProd TyInt TyBool)
  , "fst" ~: typeCheck (TmFst (TmPair (TmInt 1) (TmInt 2))) ~?= Right TyInt
  , "snd" ~: typeCheck (TmSnd (TmPair (TmInt 1) (TmInt 2))) ~?= Right TyInt
  , "identity" ~: typeCheck exIdentity ~?= Right (TyArrow TyInt TyInt)
  , "const" ~: typeCheck exConst ~?= Right (TyArrow TyInt (TyArrow TyBool TyInt))
  , "application" ~: typeCheck (TmApp exIdentity (TmInt 5)) ~?= Right TyInt
  ]

-- | Test linear terms
testLinearTerms :: Test
testLinearTerms = TestLabel "Linear Terms" $ TestList
  [ "linear lambda" ~:
      let term = TmLinLam "x" TyInt (TmVar "x")
      in typeCheck term ~?= Right (TyLollipop TyInt TyInt)

  , "tensor" ~:
      let term = TmTensor (TmInt 1) (TmInt 2)
      in typeCheck term ~?= Right (TyTensor TyInt TyInt)

  , "let tensor" ~:
      let term = TmLetTensor "x" "y" (TmTensor (TmInt 1) (TmInt 2))
                   (TmPair (TmVar "x") (TmVar "y"))
      in typeCheck term ~?= Right (TyProd TyInt TyInt)
  ]

-- | Test inductive types
testInductiveTypes :: Test
testInductiveTypes = TestLabel "Inductive Types" $ TestList
  [ "nat type" ~:
      let term = exZero
      in typeCheck term ~?= Right tyNat

  , "succ function" ~:
      typeCheck exSucc ~?= Right (TyArrow tyNat tyNat)

  , "two" ~:
      typeCheck exTwo ~?= Right tyNat

  , "list nil" ~:
      typeCheck (exNil TyInt) ~?= Right (tyList TyInt)

  , "list cons" ~:
      typeCheck (exCons TyInt) ~?= Right (TyArrow TyInt (TyArrow (tyList TyInt) (tyList TyInt)))
  ]

-- | Test coinductive types
testCoinductiveTypes :: Test
testCoinductiveTypes = TestLabel "Coinductive Types" $ TestList
  [ "stream type" ~:
      let ty = tyStream TyInt
      in ty ~?= TyNu "X" (TyProd TyInt (TyVar "X"))

  , "ones stream" ~:
      typeCheck exOnes ~?= Right (tyStream TyInt)

  , "nats function" ~:
      typeCheck exNats ~?= Right (TyArrow TyInt (tyStream TyInt))

  , "take first" ~:
      typeCheck exTake ~?= Right (TyArrow (tyStream TyInt) TyInt)
  ]

-- | Test session types
testSessionTypes :: Test
testSessionTypes = TestLabel "Session Types" $ TestList
  [ "newChan type" ~:
      let term = TmNewChan TyInt
      in typeCheck term ~?= Right (TyProc (TyTensor (TyChan TyInt) (TyChan TyInt)))

  , "return" ~:
      typeCheck (TmReturn (TmInt 42)) ~?= Right (TyProc TyInt)

  , "fork" ~:
      let term = TmFork (TmReturn TmUnit)
      in typeCheck term ~?= Right (TyProc TyUnit)
  ]

-- | Test that linear typing errors are caught
testLinearErrors :: Test
testLinearErrors = TestLabel "Linear Errors" $ TestList
  [ "unused linear var" ~:
      let term = TmLinLam "x" TyInt TmUnit
      in case typeCheck term of
           Left (LinearVariableNotUsed "x") -> True
           _ -> False
      ~?= True

  , "unbound variable" ~:
      case typeCheck (TmVar "undefined") of
        Left (UnboundVariable "undefined") -> True
        _ -> False
      ~?= True
  ]
