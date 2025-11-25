-- | Example programs in NuPi
--
-- This module contains various example programs demonstrating
-- the features of NuPi, including:
--   * Pure functional programming
--   * Coinductive types (streams)
--   * Session-typed channels
--   * Concurrent processes
module NuPi.Examples
  ( -- * Pure Examples
    exIdentity
  , exConst
  , exPair
  , exSum
    -- * Inductive Types
  , tyNat
  , exZero
  , exSucc
  , exTwo
  , tyList
  , exNil
  , exCons
    -- * Coinductive Types (Streams)
  , tyStream
  , exNats
  , exOnes
  , exTake
    -- * Session Types
  , tyIntSender
  , tyIntReceiver
  , exSendInt
  , exRecvInt
    -- * Counter Service Example
  , tyCounter
  , exCounterClient
    -- * Ping-Pong Example
  , tyPingPong
  , exPinger
  , exPonger
  ) where

import NuPi.Syntax

-- ============================================================
-- Pure Examples
-- ============================================================

-- | Identity function: λx:Int. x
exIdentity :: Term
exIdentity = TmLam "x" TyInt (TmVar "x")

-- | Const function: λx:Int. λy:Bool. x
exConst :: Term
exConst = TmLam "x" TyInt (TmLam "y" TyBool (TmVar "x"))

-- | Pair creation and projection
exPair :: Term
exPair = TmLet "p" (TmPair (TmInt 1) (TmInt 2))
           (TmPair (TmSnd (TmVar "p")) (TmFst (TmVar "p")))

-- | Sum type example
exSum :: Term
exSum =
  let sumTy = TyPlus [("Left", TyInt), ("Right", TyBool)]
  in TmCase (TmInj "Left" (TmInt 42) sumTy)
       [ ("Left", "n", TmVar "n")
       , ("Right", "b", TmInt 0)
       ]

-- ============================================================
-- Inductive Types
-- ============================================================

-- | Natural numbers: μX. 1 + X
tyNat :: Ty
tyNat = TyMu "X" (TyPlus [("Zero", TyUnit), ("Succ", TyVar "X")])

-- | Zero : Nat
exZero :: Term
exZero = TmFold tyNat (TmInj "Zero" TmUnit (TyPlus [("Zero", TyUnit), ("Succ", tyNat)]))

-- | Successor function
exSucc :: Term
exSucc = TmLam "n" tyNat
           (TmFold tyNat (TmInj "Succ" (TmVar "n") (TyPlus [("Zero", TyUnit), ("Succ", tyNat)])))

-- | Two : Nat (= Succ (Succ Zero))
exTwo :: Term
exTwo = TmApp exSucc (TmApp exSucc exZero)

-- | List type: μX. 1 + (A × X)
tyList :: Ty -> Ty
tyList a = TyMu "X" (TyPlus [("Nil", TyUnit), ("Cons", TyProd a (TyVar "X"))])

-- | Nil : List A
exNil :: Ty -> Term
exNil a = TmFold (tyList a) (TmInj "Nil" TmUnit (TyPlus [("Nil", TyUnit), ("Cons", TyProd a (tyList a))]))

-- | Cons : A → List A → List A
exCons :: Ty -> Term
exCons a = TmLam "x" a (TmLam "xs" (tyList a)
             (TmFold (tyList a)
               (TmInj "Cons" (TmPair (TmVar "x") (TmVar "xs"))
                 (TyPlus [("Nil", TyUnit), ("Cons", TyProd a (tyList a))]))))

-- ============================================================
-- Coinductive Types (Streams)
-- ============================================================

-- | Stream type: νX. A × X
tyStream :: Ty -> Ty
tyStream a = TyNu "X" (TyProd a (TyVar "X"))

-- | Infinite stream of natural numbers starting from n
-- nats : Int → Stream Int
-- nats n = coiter[Stream Int](n, λs. (s, s+1))
--
-- Note: We represent s+1 as a placeholder since we don't have arithmetic built-in
exNats :: Term
exNats = TmLam "start" TyInt
           (TmCoiter (tyStream TyInt)
             (TmVar "start")
             -- Step function: s → (s, s+1)
             -- For simplicity, this returns a pair (s, s) - incrementing would need arithmetic
             (TmLam "s" TyInt (TmPair (TmVar "s") (TmVar "s"))))

-- | Infinite stream of ones: 1, 1, 1, ...
-- ones = coiter[Stream Int]((), λ_. (1, ()))
exOnes :: Term
exOnes = TmCoiter (tyStream TyInt)
           TmUnit
           (TmLam "s" TyUnit (TmPair (TmInt 1) TmUnit))

-- | Take n elements from a stream (simplified - returns first element n times as a list)
-- This demonstrates unfoldNu
exTake :: Term
exTake = TmLam "stream" (tyStream TyInt)
           (TmLet "obs" (TmUnfoldNu (TmVar "stream"))
             (TmFst (TmVar "obs")))

-- ============================================================
-- Session Types
-- ============================================================

-- | A channel that sends an Int then ends
-- IntSender = Int ⊗ 1
tyIntSender :: Ty
tyIntSender = TyTensor TyInt TyUnit

-- | Dual: A channel that receives an Int then ends
-- IntReceiver = Int ⊸ 1
tyIntReceiver :: Ty
tyIntReceiver = TyLollipop TyInt TyUnit

-- | Send an integer on a channel
-- exSendInt : Chan(Int ⊗ 1) ⊸ Proc 1
exSendInt :: Term
exSendInt = TmLinLam "c" (TyChan tyIntSender)
              (TmBind "_" (TmSend (TmVar "c") (TmInt 42))
                (TmReturn TmUnit))

-- | Receive an integer from a channel
-- exRecvInt : Chan(Int ⊸ 1) ⊸ Proc Int
exRecvInt :: Term
exRecvInt = TmLinLam "c" (TyChan tyIntReceiver)
              (TmBind "result" (TmRecv (TmVar "c"))
                (TmLetTensor "val" "c'" (TmVar "result")
                  (TmReturn (TmVar "val"))))

-- ============================================================
-- Counter Service Example
-- ============================================================

-- | Counter protocol using external choice (&)
-- Counter = νX. &{ Inc: X, Read: Int ⊗ X, Stop: 1 }
tyCounter :: Ty
tyCounter = TyNu "X" (TyWith
  [ ("Inc", TyVar "X")
  , ("Read", TyTensor TyInt (TyVar "X"))
  , ("Stop", TyUnit)
  ])

-- | Counter client that increments twice, reads, and stops
exCounterClient :: Term
exCounterClient = TmLinLam "c" (TyChan (dual tyCounter))
  -- dual(Counter) = μX. ⊕{ Inc: X, Read: Int ⊸ X, Stop: 1 }
  (TmBind "c1" (TmSendLabel "Inc" (TmVar "c"))
    (TmBind "c2" (TmSendLabel "Inc" (TmVar "c1"))
      (TmBind "c3" (TmSendLabel "Read" (TmVar "c2"))
        (TmBind "result" (TmRecv (TmVar "c3"))
          (TmLetTensor "n" "c4" (TmVar "result")
            (TmBind "_" (TmSendLabel "Stop" (TmVar "c4"))
              (TmReturn (TmVar "n"))))))))

-- ============================================================
-- Ping-Pong Example
-- ============================================================

-- | Ping-pong protocol
-- PingPong = νX. ⊕{ Ping: &{ Pong: X }, Done: 1 }
tyPingPong :: Ty
tyPingPong = TyNu "X" (TyPlus
  [ ("Ping", TyWith [("Pong", TyVar "X")])
  , ("Done", TyUnit)
  ])

-- | Pinger: sends Ping, receives Pong, then Done
exPinger :: Term
exPinger = TmLinLam "c" (TyChan tyPingPong)
  (TmBind "c1" (TmSendLabel "Ping" (TmVar "c"))
    (TmRecvLabel (TmVar "c1")
      [ ("Pong", TmBind "c2" (TmReturn (TmVar "c"))
                   (TmBind "_" (TmSendLabel "Done" (TmVar "c2"))
                     (TmReturn TmUnit)))
      ]))

-- | Ponger: receives Ping, sends Pong, loops or done
exPonger :: Term
exPonger = TmLinLam "c" (TyChan (dual tyPingPong))
  (TmRecvLabel (TmVar "c")
    [ ("Ping", TmBind "c1" (TmSendLabel "Pong" (TmVar "c"))
                 (TmReturn TmUnit))
    , ("Done", TmReturn TmUnit)
    ])
