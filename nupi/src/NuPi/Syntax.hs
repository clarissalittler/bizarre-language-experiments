{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Core syntax definitions for NuPi
--
-- NuPi is a unified language combining:
--   * Linear types (⊗, ⊸, ⊕, &)
--   * Inductive (μ) and coinductive (ν) types
--   * Session-typed channels (Chan A)
--   * Processes (Proc A)
module NuPi.Syntax
  ( -- * Names and Variables
    Name
  , TyVar
  , ChanName
    -- * Types
  , Ty(..)
  , dual
    -- * Terms
  , Term(..)
  , Pattern(..)
  , Label
    -- * Values
  , Value(..)
  , Continuation(..)
    -- * Utilities
  , freeVars
  , freeTyVars
  , substTy
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Variable names
type Name = String

-- | Type variable names
type TyVar = String

-- | Channel names
type ChanName = String

-- | Labels for sum types / choices
type Label = String

-- | Unified type grammar
--
-- A,B ::= 1 | Int | Bool | A×B | A→B | A⊸B | A⊗B
--       | A⊕B | A&B | μX.A | νX.A | X
--       | Chan A | Proc A
data Ty
  = TyUnit                    -- ^ Unit type (1)
  | TyInt                     -- ^ Integer type
  | TyBool                    -- ^ Boolean type
  | TyProd Ty Ty              -- ^ Cartesian product (A × B) - unrestricted
  | TyArrow Ty Ty             -- ^ Regular function (A → B) - unrestricted
  | TyLollipop Ty Ty          -- ^ Linear function (A ⊸ B) - used exactly once
  | TyTensor Ty Ty            -- ^ Linear pair / send (A ⊗ B)
  | TyPlus [(Label, Ty)]      -- ^ Internal choice / sum (A ⊕ B) - labeled variant
  | TyWith [(Label, Ty)]      -- ^ External choice (A & B) - labeled variant
  | TyMu TyVar Ty             -- ^ Inductive type (μX.A)
  | TyNu TyVar Ty             -- ^ Coinductive type (νX.A)
  | TyVar TyVar               -- ^ Type variable
  | TyChan Ty                 -- ^ Channel type (Chan A)
  | TyProc Ty                 -- ^ Process returning A (Proc A)
  deriving (Eq, Show, Ord)

-- | Duality operation on types
--
-- dual(A⊗B) = dual(A) ⊸ dual(B)
-- dual(A⊸B) = dual(A) ⊗ dual(B)
-- dual(A⊕B) = dual(A) & dual(B)
-- dual(μX.A) = νX.dual(A)
-- etc.
dual :: Ty -> Ty
dual TyUnit = TyUnit
dual TyInt = TyInt
dual TyBool = TyBool
dual (TyProd a b) = TyProd (dual a) (dual b)
dual (TyArrow a b) = TyArrow (dual a) (dual b)
dual (TyLollipop a b) = TyTensor (dual a) (dual b)
dual (TyTensor a b) = TyLollipop (dual a) (dual b)
dual (TyPlus alts) = TyWith [(l, dual t) | (l, t) <- alts]
dual (TyWith alts) = TyPlus [(l, dual t) | (l, t) <- alts]
dual (TyMu x a) = TyNu x (dual a)
dual (TyNu x a) = TyMu x (dual a)
dual (TyVar x) = TyVar x
dual (TyChan a) = TyChan (dual a)
dual (TyProc a) = TyProc (dual a)

-- | Term language
--
-- Combines pure terms and process operations
data Term
  -- Pure terms
  = TmVar Name                          -- ^ Variable
  | TmUnit                              -- ^ Unit value ()
  | TmInt Integer                       -- ^ Integer literal
  | TmBool Bool                         -- ^ Boolean literal
  | TmPair Term Term                    -- ^ Pair construction (e1, e2)
  | TmFst Term                          -- ^ First projection
  | TmSnd Term                          -- ^ Second projection
  | TmLam Name Ty Term                  -- ^ Lambda abstraction
  | TmLinLam Name Ty Term               -- ^ Linear lambda abstraction
  | TmApp Term Term                     -- ^ Application
  -- Tensor (linear pair)
  | TmTensor Term Term                  -- ^ Linear pair (e1 ⊗ e2)
  | TmLetTensor Name Name Term Term     -- ^ let (x ⊗ y) = e1 in e2
  -- Sum types / internal choice
  | TmInj Label Term Ty                 -- ^ Injection with label
  | TmCase Term [(Label, Name, Term)]   -- ^ Case analysis
  -- With types / external choice
  | TmRecord [(Label, Term)]            -- ^ Record / with introduction
  | TmSelect Label Term                 -- ^ Selection from with type
  -- Recursion
  | TmFold Ty Term                      -- ^ Fold into μ type
  | TmUnfold Term                       -- ^ Unfold from μ type
  | TmCoiter Ty Term Term               -- ^ Coiteration into ν type
  | TmUnfoldNu Term                     -- ^ Observation from ν type (one step)
  | TmFix Name Ty Term                  -- ^ General recursion (for coinductive defs)
  -- Channels and processes
  | TmNewChan Ty                        -- ^ Create new channel pair: Proc (Chan A ⊗ Chan (dual A))
  | TmSend Term Term                    -- ^ send channel value : Proc (Chan A)
  | TmRecv Term                         -- ^ recv channel : Proc (T ⊗ Chan A)
  | TmClose Term                        -- ^ close channel : Proc 1
  | TmFork Term                         -- ^ fork a process : Proc 1
  | TmReturn Term                       -- ^ return : A → Proc A
  | TmBind Name Term Term               -- ^ do x <- e1; e2 (monadic bind)
  -- Labels for session choice
  | TmSendLabel Label Term              -- ^ send a label on channel
  | TmRecvLabel Term [(Label, Term)]    -- ^ receive and branch on label
  -- Let binding
  | TmLet Name Term Term                -- ^ let x = e1 in e2
  deriving (Eq, Show)

-- | Patterns for matching
data Pattern
  = PVar Name
  | PUnit
  | PPair Pattern Pattern
  | PTensor Pattern Pattern
  deriving (Eq, Show)

-- | Runtime values
data Value
  = VUnit
  | VInt Integer
  | VBool Bool
  | VPair Value Value
  | VTensor Value Value
  | VClosure Name Term Env
  | VLinClosure Name Term Env
  | VInj Label Value
  | VRecord [(Label, Value)]
  | VFold Value
  | VCoiter Value Value           -- ^ Coiterator: seed and step function
  | VChan ChanName ChannelEnd     -- ^ Channel endpoint
  | VProc (Continuation Value)    -- ^ Suspended process
  deriving (Show)

-- | Which end of a channel
data ChannelEnd = ChanPos | ChanNeg
  deriving (Eq, Show)

-- | Environment mapping names to values
type Env = [(Name, Value)]

-- | Process continuation
data Continuation a
  = Done a
  | NewChan Ty (ChanName -> Continuation a)
  | Send ChanName Value (Continuation a)
  | Recv ChanName (Value -> Continuation a)
  | SendLabel ChanName Label (Continuation a)
  | RecvLabel ChanName [(Label, Continuation a)]
  | Fork (Continuation ()) (Continuation a)
  | Close ChanName (Continuation a)

instance Show a => Show (Continuation a) where
  show (Done a) = "Done " ++ show a
  show (NewChan t _) = "NewChan " ++ show t ++ " <k>"
  show (Send c v _) = "Send " ++ c ++ " " ++ show v ++ " <k>"
  show (Recv c _) = "Recv " ++ c ++ " <k>"
  show (SendLabel c l _) = "SendLabel " ++ c ++ " " ++ l ++ " <k>"
  show (RecvLabel c _) = "RecvLabel " ++ c ++ " <k>"
  show (Fork _ _) = "Fork <k1> <k2>"
  show (Close c _) = "Close " ++ c ++ " <k>"

instance Functor Continuation where
  fmap f (Done a) = Done (f a)
  fmap f (NewChan t k) = NewChan t (fmap f . k)
  fmap f (Send c v k) = Send c v (fmap f k)
  fmap f (Recv c k) = Recv c (fmap f . k)
  fmap f (SendLabel c l k) = SendLabel c l (fmap f k)
  fmap f (RecvLabel c alts) = RecvLabel c [(l, fmap f k) | (l, k) <- alts]
  fmap f (Fork p k) = Fork p (fmap f k)
  fmap f (Close c k) = Close c (fmap f k)

-- | Get free variables of a term
freeVars :: Term -> Set Name
freeVars (TmVar x) = Set.singleton x
freeVars TmUnit = Set.empty
freeVars (TmInt _) = Set.empty
freeVars (TmBool _) = Set.empty
freeVars (TmPair e1 e2) = freeVars e1 `Set.union` freeVars e2
freeVars (TmFst e) = freeVars e
freeVars (TmSnd e) = freeVars e
freeVars (TmLam x _ e) = Set.delete x (freeVars e)
freeVars (TmLinLam x _ e) = Set.delete x (freeVars e)
freeVars (TmApp e1 e2) = freeVars e1 `Set.union` freeVars e2
freeVars (TmTensor e1 e2) = freeVars e1 `Set.union` freeVars e2
freeVars (TmLetTensor x y e1 e2) =
  freeVars e1 `Set.union` Set.delete x (Set.delete y (freeVars e2))
freeVars (TmInj _ e _) = freeVars e
freeVars (TmCase e alts) =
  freeVars e `Set.union` Set.unions [Set.delete x (freeVars t) | (_, x, t) <- alts]
freeVars (TmRecord fs) = Set.unions [freeVars e | (_, e) <- fs]
freeVars (TmSelect _ e) = freeVars e
freeVars (TmFold _ e) = freeVars e
freeVars (TmUnfold e) = freeVars e
freeVars (TmCoiter _ seed step) = freeVars seed `Set.union` freeVars step
freeVars (TmUnfoldNu e) = freeVars e
freeVars (TmFix x _ e) = Set.delete x (freeVars e)
freeVars (TmNewChan _) = Set.empty
freeVars (TmSend e1 e2) = freeVars e1 `Set.union` freeVars e2
freeVars (TmRecv e) = freeVars e
freeVars (TmClose e) = freeVars e
freeVars (TmFork e) = freeVars e
freeVars (TmReturn e) = freeVars e
freeVars (TmBind x e1 e2) = freeVars e1 `Set.union` Set.delete x (freeVars e2)
freeVars (TmSendLabel _ e) = freeVars e
freeVars (TmRecvLabel e alts) = freeVars e `Set.union` Set.unions [freeVars t | (_, t) <- alts]
freeVars (TmLet x e1 e2) = freeVars e1 `Set.union` Set.delete x (freeVars e2)

-- | Get free type variables
freeTyVars :: Ty -> Set TyVar
freeTyVars TyUnit = Set.empty
freeTyVars TyInt = Set.empty
freeTyVars TyBool = Set.empty
freeTyVars (TyProd a b) = freeTyVars a `Set.union` freeTyVars b
freeTyVars (TyArrow a b) = freeTyVars a `Set.union` freeTyVars b
freeTyVars (TyLollipop a b) = freeTyVars a `Set.union` freeTyVars b
freeTyVars (TyTensor a b) = freeTyVars a `Set.union` freeTyVars b
freeTyVars (TyPlus alts) = Set.unions [freeTyVars t | (_, t) <- alts]
freeTyVars (TyWith alts) = Set.unions [freeTyVars t | (_, t) <- alts]
freeTyVars (TyMu x a) = Set.delete x (freeTyVars a)
freeTyVars (TyNu x a) = Set.delete x (freeTyVars a)
freeTyVars (TyVar x) = Set.singleton x
freeTyVars (TyChan a) = freeTyVars a
freeTyVars (TyProc a) = freeTyVars a

-- | Substitute a type for a type variable
substTy :: TyVar -> Ty -> Ty -> Ty
substTy _ _ TyUnit = TyUnit
substTy _ _ TyInt = TyInt
substTy _ _ TyBool = TyBool
substTy x s (TyProd a b) = TyProd (substTy x s a) (substTy x s b)
substTy x s (TyArrow a b) = TyArrow (substTy x s a) (substTy x s b)
substTy x s (TyLollipop a b) = TyLollipop (substTy x s a) (substTy x s b)
substTy x s (TyTensor a b) = TyTensor (substTy x s a) (substTy x s b)
substTy x s (TyPlus alts) = TyPlus [(l, substTy x s t) | (l, t) <- alts]
substTy x s (TyWith alts) = TyWith [(l, substTy x s t) | (l, t) <- alts]
substTy x s (TyMu y a)
  | x == y    = TyMu y a
  | otherwise = TyMu y (substTy x s a)
substTy x s (TyNu y a)
  | x == y    = TyNu y a
  | otherwise = TyNu y (substTy x s a)
substTy x s (TyVar y)
  | x == y    = s
  | otherwise = TyVar y
substTy x s (TyChan a) = TyChan (substTy x s a)
substTy x s (TyProc a) = TyProc (substTy x s a)
