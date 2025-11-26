# NuPi - A Unified Language for Coinduction and Communication

NuPi is an interpreter for a language that combines:

- **Linear types** (⊗, ⊸, ⊕, &) for precise resource management
- **Inductive types** (μX.A) for finite data structures
- **Coinductive types** (νX.A) for infinite data (streams, processes)
- **Session-typed channels** (Chan A, Proc A) for typed communication
- **Duality** for bidirectional channel typing

## Language Overview

### Unified Type Grammar

```
A, B ::= 1           -- Unit
       | Int         -- Integers
       | Bool        -- Booleans
       | A × B       -- Cartesian product (unrestricted)
       | A → B       -- Function (unrestricted)
       | A ⊸ B       -- Linear function (used exactly once)
       | A ⊗ B       -- Tensor / linear pair
       | ⊕{l: A ...} -- Internal choice (sum)
       | &{l: A ...} -- External choice
       | μX.A        -- Inductive type
       | νX.A        -- Coinductive type
       | X           -- Type variable
       | Chan A      -- Channel endpoint
       | Proc A      -- Process returning A
```

### Duality

The duality operation ensures that channel endpoints have compatible types:

```
dual(A ⊗ B) = dual(A) ⊸ dual(B)
dual(A ⊸ B) = dual(A) ⊗ dual(B)
dual(⊕{...}) = &{...}  (with dualized components)
dual(μX.A) = νX.dual(A)
```

### Key Constructs

#### Coinductive Streams

```haskell
-- Stream type: νX. A × X
tyStream :: Ty -> Ty
tyStream a = TyNu "X" (TyProd a (TyVar "X"))

-- Infinite stream of ones
exOnes :: Term
exOnes = TmCoiter (tyStream TyInt)
           TmUnit
           (TmLam "s" TyUnit (TmPair (TmInt 1) TmUnit))
```

#### Session-Typed Channels

```haskell
-- Counter protocol: νX. &{ Inc: X, Read: Int ⊗ X, Stop: 1 }
tyCounter :: Ty
tyCounter = TyNu "X" (TyWith
  [ ("Inc", TyVar "X")
  , ("Read", TyTensor TyInt (TyVar "X"))
  , ("Stop", TyUnit)
  ])
```

## Building

Requires GHC 9.4+ and Cabal 3.0+.

```bash
# Build the project
cabal build

# Run the executable
cabal run nupi

# Run tests
cabal test
```

## Project Structure

```
nupi/
├── src/
│   └── NuPi/
│       ├── Syntax.hs      -- AST definitions
│       ├── Types.hs       -- Type utilities
│       ├── TypeCheck.hs   -- Linear type checker
│       ├── Eval.hs        -- Interpreter
│       ├── Pretty.hs      -- Pretty printing
│       └── Examples.hs    -- Example programs
├── app/
│   └── Main.hs            -- CLI entry point
├── test/
│   ├── Main.hs            -- Test runner
│   ├── TypeCheckTests.hs  -- Type checking tests
│   ├── EvalTests.hs       -- Evaluation tests
│   └── IntegrationTests.hs-- End-to-end tests
├── examples/
│   ├── Streams.hs         -- Stream examples
│   └── Sessions.hs        -- Session type examples
└── nupi.cabal             -- Build configuration
```

## Example Programs

### Natural Numbers (Inductive)

```haskell
-- Nat = μX. 1 + X
tyNat = TyMu "X" (TyPlus [("Zero", TyUnit), ("Succ", TyVar "X")])

-- Zero : Nat
exZero = TmFold tyNat (TmInj "Zero" TmUnit ...)

-- Succ : Nat → Nat
exSucc = TmLam "n" tyNat (TmFold tyNat (TmInj "Succ" (TmVar "n") ...))
```

### Infinite Streams (Coinductive)

```haskell
-- Stream Int = νX. Int × X
tyStream TyInt = TyNu "X" (TyProd TyInt (TyVar "X"))

-- ones : Stream Int  (1, 1, 1, ...)
exOnes = TmCoiter (tyStream TyInt) TmUnit (TmLam "s" TyUnit (TmPair (TmInt 1) TmUnit))

-- take first element
head s = TmFst (TmUnfoldNu s)
```

### Session-Typed Communication

```haskell
-- Client sends Int, receives Int
-- Chan(Int ⊗ (Int ⊸ 1)) ⊸ Proc Int
client c =
  TmBind "c1" (TmSend c (TmInt 42))
    (TmBind "result" (TmRecv c1)
      (TmReturn ...))
```

## Type System

NuPi uses a split context system:

```
Γ ; Δ ⊢ e : A
```

- **Γ**: Unrestricted context (variables can be used any number of times)
- **Δ**: Linear context (variables must be used exactly once)

Key typing rules ensure:

1. Linear variables are used exactly once
2. Channels follow their session protocol
3. Duality is preserved in communication

## Design Philosophy

NuPi eliminates the traditional stratification between:

- Data types and session types → unified type grammar
- Terms and processes → unified term language
- Induction and coinduction → μ and ν in same grammar

As the design notes: "Session types are just linear formulas you stick under `Chan`. Everything—streams, channels, protocols, processes—is a term or type in that one system."

## References

- Session Types and Linear Logic
- Coinduction in Type Theory
- Linear Type Systems
