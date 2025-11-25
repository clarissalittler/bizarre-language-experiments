{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Type checker for NuPi with linear typing
--
-- The judgment form is: Γ ; Δ ⊢ e : A
-- where Γ is the unrestricted context and Δ is the linear context
module NuPi.TypeCheck
  ( -- * Type checking
    typeCheck
  , typeCheckTerm
  , TypeError(..)
  , TCResult
    -- * Contexts
  , Ctx(..)
  , emptyCtx
  ) where

import NuPi.Syntax
import NuPi.Types

import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Type errors
data TypeError
  = UnboundVariable Name
  | TypeMismatch Ty Ty             -- Expected, Got
  | LinearVariableNotUsed Name
  | LinearVariableUsedMultipleTimes Name
  | ExpectedFunction Ty
  | ExpectedLinearFunction Ty
  | ExpectedPair Ty
  | ExpectedTensor Ty
  | ExpectedSum Ty
  | ExpectedWith Ty
  | ExpectedMu Ty
  | ExpectedNu Ty
  | ExpectedChannel Ty
  | ExpectedProcess Ty
  | LabelNotFound Label Ty
  | DuplicateLabel Label
  | NonLinearInLinearContext Name Ty
  | CannotDuplicateLinear Name Ty
  | ContextNotEmpty (Map Name Ty)
  | Other String
  deriving (Eq, Show)

-- | Type checking context
data Ctx = Ctx
  { ctxUnrestricted :: Map Name Ty    -- ^ Γ: unrestricted variables
  , ctxLinear       :: Map Name Ty    -- ^ Δ: linear variables
  , ctxUsed         :: Set Name       -- ^ Track which linear vars have been used
  , ctxFreshCounter :: Int            -- ^ For generating fresh names
  } deriving (Show)

-- | Empty context
emptyCtx :: Ctx
emptyCtx = Ctx Map.empty Map.empty Set.empty 0

-- | Type checking monad
newtype TC a = TC { unTC :: ExceptT TypeError (State Ctx) a }
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState Ctx)

-- | Result of type checking
type TCResult = Either TypeError Ty

-- | Run the type checker
runTC :: TC a -> Ctx -> (Either TypeError a, Ctx)
runTC tc ctx = runState (runExceptT (unTC tc)) ctx

-- | Type check a term in an empty context
typeCheck :: Term -> TCResult
typeCheck t = fst $ runTC (typeCheckTerm t >>= checkLinearUsed) emptyCtx

-- | Check that all linear variables have been used
checkLinearUsed :: Ty -> TC Ty
checkLinearUsed ty = do
  ctx <- get
  let unused = Map.keysSet (ctxLinear ctx) `Set.difference` ctxUsed ctx
  unless (Set.null unused) $
    throwError $ LinearVariableNotUsed (Set.elemAt 0 unused)
  return ty

-- | Type check a term
typeCheckTerm :: Term -> TC Ty

-- Variables
typeCheckTerm (TmVar x) = do
  ctx <- get
  -- Check linear context first
  case Map.lookup x (ctxLinear ctx) of
    Just ty -> do
      -- Check if already used
      when (x `Set.member` ctxUsed ctx) $
        throwError $ LinearVariableUsedMultipleTimes x
      -- Mark as used
      modify $ \c -> c { ctxUsed = Set.insert x (ctxUsed c) }
      return ty
    Nothing ->
      -- Check unrestricted context
      case Map.lookup x (ctxUnrestricted ctx) of
        Just ty -> return ty
        Nothing -> throwError $ UnboundVariable x

-- Unit
typeCheckTerm TmUnit = return TyUnit

-- Literals
typeCheckTerm (TmInt _) = return TyInt
typeCheckTerm (TmBool _) = return TyBool

-- Pairs (unrestricted)
typeCheckTerm (TmPair e1 e2) = do
  t1 <- typeCheckTerm e1
  t2 <- typeCheckTerm e2
  return $ TyProd t1 t2

typeCheckTerm (TmFst e) = do
  t <- typeCheckTerm e
  case t of
    TyProd t1 _ -> return t1
    _ -> throwError $ ExpectedPair t

typeCheckTerm (TmSnd e) = do
  t <- typeCheckTerm e
  case t of
    TyProd _ t2 -> return t2
    _ -> throwError $ ExpectedPair t

-- Lambda (unrestricted function)
typeCheckTerm (TmLam x ty body) = do
  ctx <- get
  -- Add x to unrestricted context
  put $ ctx { ctxUnrestricted = Map.insert x ty (ctxUnrestricted ctx) }
  bodyTy <- typeCheckTerm body
  -- Remove x from context
  modify $ \c -> c { ctxUnrestricted = Map.delete x (ctxUnrestricted c) }
  return $ TyArrow ty bodyTy

-- Linear lambda
typeCheckTerm (TmLinLam x ty body) = do
  ctx <- get
  -- Add x to linear context
  put $ ctx { ctxLinear = Map.insert x ty (ctxLinear ctx) }
  bodyTy <- typeCheckTerm body
  -- Check x was used
  ctx' <- get
  unless (x `Set.member` ctxUsed ctx') $
    throwError $ LinearVariableNotUsed x
  -- Remove x from context and used set
  modify $ \c -> c { ctxLinear = Map.delete x (ctxLinear c)
                   , ctxUsed = Set.delete x (ctxUsed c) }
  return $ TyLollipop ty bodyTy

-- Application
typeCheckTerm (TmApp e1 e2) = do
  t1 <- typeCheckTerm e1
  case t1 of
    TyArrow argTy resTy -> do
      t2 <- typeCheckTerm e2
      unless (tyEqual t2 argTy) $
        throwError $ TypeMismatch argTy t2
      return resTy
    TyLollipop argTy resTy -> do
      t2 <- typeCheckTerm e2
      unless (tyEqual t2 argTy) $
        throwError $ TypeMismatch argTy t2
      return resTy
    _ -> throwError $ ExpectedFunction t1

-- Tensor (linear pair)
typeCheckTerm (TmTensor e1 e2) = do
  t1 <- typeCheckTerm e1
  t2 <- typeCheckTerm e2
  return $ TyTensor t1 t2

typeCheckTerm (TmLetTensor x y e1 e2) = do
  t1 <- typeCheckTerm e1
  case t1 of
    TyTensor tx ty -> do
      ctx <- get
      -- Add both as linear
      let isLinX = isLinear tx
      let isLinY = isLinear ty
      put $ ctx { ctxLinear = Map.insert x tx $ Map.insert y ty $ ctxLinear ctx }
      bodyTy <- typeCheckTerm e2
      -- Check usage
      ctx' <- get
      when isLinX $ unless (x `Set.member` ctxUsed ctx') $
        throwError $ LinearVariableNotUsed x
      when isLinY $ unless (y `Set.member` ctxUsed ctx') $
        throwError $ LinearVariableNotUsed y
      -- Cleanup
      modify $ \c -> c { ctxLinear = Map.delete x $ Map.delete y $ ctxLinear c
                       , ctxUsed = Set.delete x $ Set.delete y $ ctxUsed c }
      return bodyTy
    _ -> throwError $ ExpectedTensor t1

-- Sum types
typeCheckTerm (TmInj lbl e ty) = do
  case ty of
    TyPlus alts ->
      case lookup lbl alts of
        Just expectedTy -> do
          t <- typeCheckTerm e
          unless (tyEqual t expectedTy) $
            throwError $ TypeMismatch expectedTy t
          return ty
        Nothing -> throwError $ LabelNotFound lbl ty
    _ -> throwError $ ExpectedSum ty

typeCheckTerm (TmCase e alts) = do
  t <- typeCheckTerm e
  case t of
    TyPlus tyAlts -> do
      -- Check all labels are covered
      let labels = map fst3 alts
      let tyLabels = map fst tyAlts
      unless (Set.fromList labels == Set.fromList tyLabels) $
        throwError $ Other "Case alternatives don't match sum type"
      -- Type check each alternative
      ctx <- get
      altTys <- forM alts $ \(lbl, x, body) -> do
        case lookup lbl tyAlts of
          Just varTy -> do
            put $ ctx { ctxLinear = Map.insert x varTy (ctxLinear ctx) }
            bodyTy <- typeCheckTerm body
            ctx' <- get
            when (isLinear varTy) $ unless (x `Set.member` ctxUsed ctx') $
              throwError $ LinearVariableNotUsed x
            modify $ \c -> c { ctxLinear = Map.delete x (ctxLinear c)
                             , ctxUsed = Set.delete x (ctxUsed c) }
            return bodyTy
          Nothing -> throwError $ LabelNotFound lbl t
      -- All alternatives must have same type
      case altTys of
        [] -> throwError $ Other "Empty case"
        (ty':tys) -> do
          unless (all (tyEqual ty') tys) $
            throwError $ Other "Case alternatives have different types"
          return ty'
    _ -> throwError $ ExpectedSum t
  where
    fst3 (a, _, _) = a

-- With types (external choice)
typeCheckTerm (TmRecord fields) = do
  fieldTys <- forM fields $ \(lbl, e) -> do
    ty <- typeCheckTerm e
    return (lbl, ty)
  return $ TyWith fieldTys

typeCheckTerm (TmSelect lbl e) = do
  t <- typeCheckTerm e
  case t of
    TyWith alts ->
      case lookup lbl alts of
        Just ty -> return ty
        Nothing -> throwError $ LabelNotFound lbl t
    _ -> throwError $ ExpectedWith t

-- Inductive types
typeCheckTerm (TmFold ty e) = do
  case ty of
    TyMu _ _ -> do
      case unfoldMu ty of
        Just unfolded -> do
          t <- typeCheckTerm e
          unless (tyEqual t unfolded) $
            throwError $ TypeMismatch unfolded t
          return ty
        Nothing -> throwError $ ExpectedMu ty
    _ -> throwError $ ExpectedMu ty

typeCheckTerm (TmUnfold e) = do
  t <- typeCheckTerm e
  case unfoldMu t of
    Just unfolded -> return unfolded
    Nothing -> throwError $ ExpectedMu t

-- Coinductive types
typeCheckTerm (TmCoiter ty seed step) = do
  case ty of
    TyNu x body -> do
      seedTy <- typeCheckTerm seed
      stepTy <- typeCheckTerm step
      -- step : S → body[S/X]
      -- where S is the seed type
      let expectedStepTy = TyArrow seedTy (substTy x seedTy body)
      unless (tyEqual stepTy expectedStepTy) $
        throwError $ TypeMismatch expectedStepTy stepTy
      return ty
    _ -> throwError $ ExpectedNu ty

typeCheckTerm (TmUnfoldNu e) = do
  t <- typeCheckTerm e
  case unfoldNu t of
    Just unfolded -> return unfolded
    Nothing -> throwError $ ExpectedNu t

-- General recursion (for coinductive definitions)
typeCheckTerm (TmFix x ty body) = do
  ctx <- get
  put $ ctx { ctxUnrestricted = Map.insert x ty (ctxUnrestricted ctx) }
  bodyTy <- typeCheckTerm body
  unless (tyEqual bodyTy ty) $
    throwError $ TypeMismatch ty bodyTy
  modify $ \c -> c { ctxUnrestricted = Map.delete x (ctxUnrestricted c) }
  return ty

-- Channel operations
typeCheckTerm (TmNewChan ty) = do
  -- newChan : Proc (Chan A ⊗ Chan (dual A))
  return $ TyProc $ TyTensor (TyChan ty) (TyChan (dual ty))

typeCheckTerm (TmSend chan val) = do
  chanTy <- typeCheckTerm chan
  case chanTy of
    TyChan (TyTensor sendTy contTy) -> do
      valTy <- typeCheckTerm val
      unless (tyEqual valTy sendTy) $
        throwError $ TypeMismatch sendTy valTy
      return $ TyProc (TyChan contTy)
    _ -> throwError $ Other $ "Expected channel with send type, got " ++ show chanTy

typeCheckTerm (TmRecv chan) = do
  chanTy <- typeCheckTerm chan
  case chanTy of
    TyChan (TyLollipop recvTy contTy) -> do
      return $ TyProc $ TyTensor recvTy (TyChan contTy)
    _ -> throwError $ Other $ "Expected channel with receive type, got " ++ show chanTy

typeCheckTerm (TmClose chan) = do
  chanTy <- typeCheckTerm chan
  case chanTy of
    TyChan TyUnit -> return $ TyProc TyUnit
    _ -> throwError $ Other $ "Expected channel with unit type, got " ++ show chanTy

typeCheckTerm (TmFork e) = do
  t <- typeCheckTerm e
  case t of
    TyProc _ -> return $ TyProc TyUnit
    _ -> throwError $ ExpectedProcess t

typeCheckTerm (TmReturn e) = do
  t <- typeCheckTerm e
  return $ TyProc t

typeCheckTerm (TmBind x e1 e2) = do
  t1 <- typeCheckTerm e1
  case t1 of
    TyProc resultTy -> do
      ctx <- get
      -- Bind result to linear context
      put $ ctx { ctxLinear = Map.insert x resultTy (ctxLinear ctx) }
      t2 <- typeCheckTerm e2
      case t2 of
        TyProc _ -> do
          ctx' <- get
          when (isLinear resultTy) $ unless (x `Set.member` ctxUsed ctx') $
            throwError $ LinearVariableNotUsed x
          modify $ \c -> c { ctxLinear = Map.delete x (ctxLinear c)
                           , ctxUsed = Set.delete x (ctxUsed c) }
          return t2
        _ -> throwError $ ExpectedProcess t2
    _ -> throwError $ ExpectedProcess t1

-- Label sending/receiving for session choice
typeCheckTerm (TmSendLabel lbl chan) = do
  chanTy <- typeCheckTerm chan
  case chanTy of
    TyChan (TyPlus alts) ->
      case lookup lbl alts of
        Just contTy -> return $ TyProc (TyChan contTy)
        Nothing -> throwError $ LabelNotFound lbl (TyPlus alts)
    _ -> throwError $ Other $ "Expected channel with sum type, got " ++ show chanTy

typeCheckTerm (TmRecvLabel chan alts) = do
  chanTy <- typeCheckTerm chan
  case chanTy of
    TyChan (TyWith tyAlts) -> do
      -- Check all labels match
      let labels = map fst alts
      let tyLabels = map fst tyAlts
      unless (Set.fromList labels == Set.fromList tyLabels) $
        throwError $ Other "Label mismatch in receive"
      -- Type each branch
      ctx <- get
      branchTys <- forM alts $ \(lbl, body) -> do
        case lookup lbl tyAlts of
          Just contTy -> do
            -- The continuation channel has type Chan contTy
            -- We need to check body expecting that channel
            put ctx  -- Reset to original context for each branch
            bodyTy <- typeCheckTerm body
            case bodyTy of
              TyProc resTy -> return resTy
              _ -> throwError $ ExpectedProcess bodyTy
          Nothing -> throwError $ LabelNotFound lbl chanTy
      case branchTys of
        [] -> throwError $ Other "Empty receive"
        (ty:tys) -> do
          unless (all (tyEqual ty) tys) $
            throwError $ Other "Branch types don't match"
          return $ TyProc ty
    _ -> throwError $ Other $ "Expected channel with & type, got " ++ show chanTy

-- Let binding
typeCheckTerm (TmLet x e1 e2) = do
  t1 <- typeCheckTerm e1
  ctx <- get
  if isLinear t1
    then put $ ctx { ctxLinear = Map.insert x t1 (ctxLinear ctx) }
    else put $ ctx { ctxUnrestricted = Map.insert x t1 (ctxUnrestricted ctx) }
  t2 <- typeCheckTerm e2
  ctx' <- get
  when (isLinear t1) $ unless (x `Set.member` ctxUsed ctx') $
    throwError $ LinearVariableNotUsed x
  modify $ \c -> c { ctxLinear = Map.delete x (ctxLinear c)
                   , ctxUnrestricted = Map.delete x (ctxUnrestricted c)
                   , ctxUsed = Set.delete x (ctxUsed c) }
  return t2
