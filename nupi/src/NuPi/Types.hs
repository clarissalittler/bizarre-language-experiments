{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Type utilities for NuPi
module NuPi.Types
  ( -- * Type checking utilities
    isLinear
  , isUnrestricted
  , unfoldMu
  , unfoldNu
  , tyEqual
    -- * Type context
  , TyCtx
  , emptyTyCtx
  , extendTyCtx
  , lookupTy
  ) where

import NuPi.Syntax

import Data.Map (Map)
import qualified Data.Map as Map

-- | Type context mapping type variables to their kinds/bounds
type TyCtx = Map TyVar ()

-- | Empty type context
emptyTyCtx :: TyCtx
emptyTyCtx = Map.empty

-- | Extend the type context
extendTyCtx :: TyVar -> TyCtx -> TyCtx
extendTyCtx x = Map.insert x ()

-- | Look up a type variable
lookupTy :: TyVar -> TyCtx -> Maybe ()
lookupTy = Map.lookup

-- | Check if a type is linear (must be used exactly once)
--
-- Linear types include:
--   - Tensor products (A ⊗ B)
--   - Linear functions (A ⊸ B)
--   - Channel types (Chan A)
--   - Process types (Proc A)
isLinear :: Ty -> Bool
isLinear TyUnit = False
isLinear TyInt = False
isLinear TyBool = False
isLinear (TyProd _ _) = False       -- Cartesian product is unrestricted
isLinear (TyArrow _ _) = False      -- Regular arrow is unrestricted
isLinear (TyLollipop _ _) = True    -- Linear function
isLinear (TyTensor _ _) = True      -- Linear pair
isLinear (TyPlus _) = True          -- Sum is linear
isLinear (TyWith _) = True          -- With is linear
isLinear (TyMu _ a) = isLinear a    -- Depends on body
isLinear (TyNu _ a) = isLinear a    -- Depends on body
isLinear (TyVar _) = False          -- Type variables are unrestricted by default
isLinear (TyChan _) = True          -- Channels are linear
isLinear (TyProc _) = True          -- Processes are linear

-- | Check if a type is unrestricted (can be freely copied/discarded)
isUnrestricted :: Ty -> Bool
isUnrestricted = not . isLinear

-- | Unfold an inductive type (μX.A → A[μX.A/X])
unfoldMu :: Ty -> Maybe Ty
unfoldMu t@(TyMu x a) = Just (substTy x t a)
unfoldMu _ = Nothing

-- | Unfold a coinductive type (νX.A → A[νX.A/X])
unfoldNu :: Ty -> Maybe Ty
unfoldNu t@(TyNu x a) = Just (substTy x t a)
unfoldNu _ = Nothing

-- | Type equality (up to alpha-equivalence)
tyEqual :: Ty -> Ty -> Bool
tyEqual TyUnit TyUnit = True
tyEqual TyInt TyInt = True
tyEqual TyBool TyBool = True
tyEqual (TyProd a1 b1) (TyProd a2 b2) = tyEqual a1 a2 && tyEqual b1 b2
tyEqual (TyArrow a1 b1) (TyArrow a2 b2) = tyEqual a1 a2 && tyEqual b1 b2
tyEqual (TyLollipop a1 b1) (TyLollipop a2 b2) = tyEqual a1 a2 && tyEqual b1 b2
tyEqual (TyTensor a1 b1) (TyTensor a2 b2) = tyEqual a1 a2 && tyEqual b1 b2
tyEqual (TyPlus alts1) (TyPlus alts2) =
  length alts1 == length alts2 &&
  all (\((l1, t1), (l2, t2)) -> l1 == l2 && tyEqual t1 t2) (zip alts1 alts2)
tyEqual (TyWith alts1) (TyWith alts2) =
  length alts1 == length alts2 &&
  all (\((l1, t1), (l2, t2)) -> l1 == l2 && tyEqual t1 t2) (zip alts1 alts2)
tyEqual (TyMu x1 a1) (TyMu x2 a2) =
  -- Alpha equivalence: μX.A ≡ μY.A[Y/X]
  tyEqual a1 (substTy x2 (TyVar x1) a2)
tyEqual (TyNu x1 a1) (TyNu x2 a2) =
  tyEqual a1 (substTy x2 (TyVar x1) a2)
tyEqual (TyVar x1) (TyVar x2) = x1 == x2
tyEqual (TyChan a1) (TyChan a2) = tyEqual a1 a2
tyEqual (TyProc a1) (TyProc a2) = tyEqual a1 a2
tyEqual _ _ = False
