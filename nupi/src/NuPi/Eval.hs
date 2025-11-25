{-# LANGUAGE LambdaCase #-}

-- | Interpreter for NuPi
--
-- This module provides:
--   * Pure term evaluation
--   * Process/channel execution via a scheduler
module NuPi.Eval
  ( -- * Evaluation
    eval
  , evalPure
  , EvalError(..)
    -- * Process execution
  , runProcess
  , runNetwork
  , Process(..)
  , Network
  , Message(..)
  ) where

import NuPi.Syntax

import Control.Monad (forM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | Evaluation errors
data EvalError
  = UnboundVar Name
  | TypeErrorRuntime String
  | ChannelError String
  | StuckProcess String
  deriving (Eq, Show)

-- | Result type
type EvalResult = Either EvalError Value

-- | Environment
type Env = [(Name, Value)]

-- | Look up a variable in the environment
lookupEnv :: Name -> Env -> Maybe Value
lookupEnv x env = lookup x env

-- | Extend the environment
extendEnv :: Name -> Value -> Env -> Env
extendEnv x v env = (x, v) : env

-- | Evaluate a term to a value
eval :: Term -> EvalResult
eval = evalWithEnv []

-- | Evaluate with an environment
evalWithEnv :: Env -> Term -> EvalResult
evalWithEnv env term = case term of
  TmVar x ->
    case lookupEnv x env of
      Just v -> Right v
      Nothing -> Left $ UnboundVar x

  TmUnit -> Right VUnit

  TmInt n -> Right $ VInt n

  TmBool b -> Right $ VBool b

  TmPair e1 e2 -> do
    v1 <- evalWithEnv env e1
    v2 <- evalWithEnv env e2
    Right $ VPair v1 v2

  TmFst e -> do
    v <- evalWithEnv env e
    case v of
      VPair v1 _ -> Right v1
      _ -> Left $ TypeErrorRuntime "Expected pair in fst"

  TmSnd e -> do
    v <- evalWithEnv env e
    case v of
      VPair _ v2 -> Right v2
      _ -> Left $ TypeErrorRuntime "Expected pair in snd"

  TmLam x ty body ->
    Right $ VClosure x body env

  TmLinLam x ty body ->
    Right $ VLinClosure x body env

  TmApp e1 e2 -> do
    v1 <- evalWithEnv env e1
    v2 <- evalWithEnv env e2
    case v1 of
      VClosure x body closureEnv ->
        evalWithEnv (extendEnv x v2 closureEnv) body
      VLinClosure x body closureEnv ->
        evalWithEnv (extendEnv x v2 closureEnv) body
      _ -> Left $ TypeErrorRuntime "Expected function in application"

  TmTensor e1 e2 -> do
    v1 <- evalWithEnv env e1
    v2 <- evalWithEnv env e2
    Right $ VTensor v1 v2

  TmLetTensor x y e1 e2 -> do
    v1 <- evalWithEnv env e1
    case v1 of
      VTensor vx vy ->
        evalWithEnv (extendEnv x vx $ extendEnv y vy env) e2
      _ -> Left $ TypeErrorRuntime "Expected tensor in let"

  TmInj lbl e _ -> do
    v <- evalWithEnv env e
    Right $ VInj lbl v

  TmCase e alts -> do
    v <- evalWithEnv env e
    case v of
      VInj lbl val ->
        case findAlt lbl alts of
          Just (x, body) -> evalWithEnv (extendEnv x val env) body
          Nothing -> Left $ TypeErrorRuntime $ "No matching case for " ++ lbl
      _ -> Left $ TypeErrorRuntime "Expected injection in case"

  TmRecord fields -> do
    vals <- forM fields $ \(lbl, e') -> do
      v <- evalWithEnv env e'
      return (lbl, v)
    Right $ VRecord vals

  TmSelect lbl e -> do
    v <- evalWithEnv env e
    case v of
      VRecord fields ->
        case lookup lbl fields of
          Just val -> Right val
          Nothing -> Left $ TypeErrorRuntime $ "Field not found: " ++ lbl
      _ -> Left $ TypeErrorRuntime "Expected record in select"

  TmFold _ e -> do
    v <- evalWithEnv env e
    Right $ VFold v

  TmUnfold e -> do
    v <- evalWithEnv env e
    case v of
      VFold inner -> Right inner
      _ -> Left $ TypeErrorRuntime "Expected fold in unfold"

  TmCoiter _ seed step -> do
    seedVal <- evalWithEnv env seed
    stepVal <- evalWithEnv env step
    Right $ VCoiter seedVal stepVal

  TmUnfoldNu e -> do
    v <- evalWithEnv env e
    case v of
      VCoiter seed step ->
        -- Apply step to seed to get one step of unfolding
        case step of
          VClosure x body closureEnv ->
            evalWithEnv (extendEnv x seed closureEnv) body
          VLinClosure x body closureEnv ->
            evalWithEnv (extendEnv x seed closureEnv) body
          _ -> Left $ TypeErrorRuntime "Expected function in coiter step"
      _ -> Left $ TypeErrorRuntime "Expected coiter in unfoldNu"

  TmFix x _ body ->
    -- Create a recursive closure
    let env' = extendEnv x (VClosure "_" (TmFix x undefined body) env) env
    in evalWithEnv env' body

  TmLet x e1 e2 -> do
    v1 <- evalWithEnv env e1
    evalWithEnv (extendEnv x v1 env) e2

  -- Process operations - these build continuations
  TmNewChan ty ->
    Right $ VProc $ NewChan ty $ \name ->
      Done $ VTensor (VChan name ChanPos) (VChan name ChanNeg)

  TmSend chanExpr valExpr -> do
    chanVal <- evalWithEnv env chanExpr
    valVal <- evalWithEnv env valExpr
    case chanVal of
      VChan name end ->
        Right $ VProc $ Send name valVal $ Done $ VChan name end
      _ -> Left $ TypeErrorRuntime "Expected channel in send"

  TmRecv chanExpr -> do
    chanVal <- evalWithEnv env chanExpr
    case chanVal of
      VChan name end ->
        Right $ VProc $ Recv name $ \v ->
          Done $ VTensor v (VChan name end)
      _ -> Left $ TypeErrorRuntime "Expected channel in recv"

  TmClose chanExpr -> do
    chanVal <- evalWithEnv env chanExpr
    case chanVal of
      VChan name _ ->
        Right $ VProc $ Close name $ Done VUnit
      _ -> Left $ TypeErrorRuntime "Expected channel in close"

  TmFork e -> do
    v <- evalWithEnv env e
    case v of
      VProc k -> Right $ VProc $ Fork (fmap (const ()) k) (Done VUnit)
      _ -> Left $ TypeErrorRuntime "Expected process in fork"

  TmReturn e -> do
    v <- evalWithEnv env e
    Right $ VProc $ Done v

  TmBind x e1 e2 -> do
    v1 <- evalWithEnv env e1
    case v1 of
      VProc k -> Right $ VProc $ bindCont k (\val ->
        case evalWithEnv (extendEnv x val env) e2 of
          Right (VProc k') -> k'
          Right _ -> Done VUnit  -- Should be an error, but simplified
          Left _ -> Done VUnit)
      _ -> Left $ TypeErrorRuntime "Expected process in bind"

  TmSendLabel lbl chanExpr -> do
    chanVal <- evalWithEnv env chanExpr
    case chanVal of
      VChan name end ->
        Right $ VProc $ SendLabel name lbl $ Done $ VChan name end
      _ -> Left $ TypeErrorRuntime "Expected channel in sendLabel"

  TmRecvLabel chanExpr alts -> do
    chanVal <- evalWithEnv env chanExpr
    case chanVal of
      VChan name end -> do
        altConts <- forM alts $ \(lbl, body) -> do
          v <- evalWithEnv env body
          case v of
            VProc k -> return (lbl, k)
            _ -> Left $ TypeErrorRuntime "Expected process in recvLabel branch"
        Right $ VProc $ RecvLabel name altConts
      _ -> Left $ TypeErrorRuntime "Expected channel in recvLabel"

-- Helper to find alternative in case
findAlt :: Label -> [(Label, Name, Term)] -> Maybe (Name, Term)
findAlt lbl alts = case filter (\(l, _, _) -> l == lbl) alts of
  [(_, x, t)] -> Just (x, t)
  _ -> Nothing

-- | Bind a continuation
bindCont :: Continuation a -> (a -> Continuation b) -> Continuation b
bindCont (Done a) f = f a
bindCont (NewChan ty k) f = NewChan ty (\name -> bindCont (k name) f)
bindCont (Send c v k) f = Send c v (bindCont k f)
bindCont (Recv c k) f = Recv c (\v -> bindCont (k v) f)
bindCont (SendLabel c l k) f = SendLabel c l (bindCont k f)
bindCont (RecvLabel c alts) f = RecvLabel c [(l, bindCont k f) | (l, k) <- alts]
bindCont (Fork p k) f = Fork p (bindCont k f)
bindCont (Close c k) f = Close c (bindCont k f)

-- | Evaluate a pure term (no process operations)
evalPure :: Term -> EvalResult
evalPure = eval

-- | A running process
data Process = Process
  { procId   :: Int
  , procCont :: Continuation Value
  } deriving (Show)

-- | A network of processes
type Network = [Process]

-- | Messages in transit
data Message = Message
  { msgChan  :: ChanName
  , msgValue :: Either Label Value  -- Either a label or a value
  } deriving (Show)

-- | Channel state
data ChanState = ChanState
  { chanQueue :: [Either Label Value]  -- Queue of pending messages
  } deriving (Show)

-- | Network state
data NetState = NetState
  { netProcesses :: [Process]
  , netChannels  :: Map ChanName ChanState
  , netNextChan  :: Int
  , netNextProc  :: Int
  , netResults   :: Map Int Value  -- Results from completed processes
  } deriving (Show)

-- | Initial network state
initNetState :: NetState
initNetState = NetState [] Map.empty 0 0 Map.empty

-- | Run a single process
runProcess :: Term -> Either EvalError Value
runProcess term = do
  v <- eval term
  case v of
    VProc (Done result) -> Right result
    VProc _ -> Left $ StuckProcess "Process requires network execution"
    other -> Right other

-- | Run a network of processes
runNetwork :: [Term] -> Either EvalError [Value]
runNetwork terms = do
  procs <- mapM evalToProcess terms
  let state0 = foldr addProcess initNetState procs
  finalState <- runScheduler state0
  return $ map snd $ Map.toList $ netResults finalState
  where
    evalToProcess t = do
      v <- eval t
      case v of
        VProc k -> Right k
        _ -> Left $ TypeErrorRuntime "Expected process"

    addProcess k state =
      let pid = netNextProc state
          proc = Process pid k
      in state { netProcesses = proc : netProcesses state
               , netNextProc = pid + 1 }

-- | Main scheduler loop
runScheduler :: NetState -> Either EvalError NetState
runScheduler state
  | null (netProcesses state) = Right state
  | otherwise = do
      -- Try to make progress
      (state', progressed) <- stepNetwork state
      if progressed
        then runScheduler state'
        else
          -- Check if all processes are done or truly stuck
          if all isDone (netProcesses state')
            then Right state'
            else Left $ StuckProcess "Deadlock detected"

-- | Check if a process is done
isDone :: Process -> Bool
isDone (Process _ (Done _)) = True
isDone _ = False

-- | Try to step the network
stepNetwork :: NetState -> Either EvalError (NetState, Bool)
stepNetwork state = tryStepProcesses (netProcesses state) [] state False

-- | Try to step each process
tryStepProcesses :: [Process] -> [Process] -> NetState -> Bool -> Either EvalError (NetState, Bool)
tryStepProcesses [] done state progressed =
  Right (state { netProcesses = done }, progressed)
tryStepProcesses (p:ps) done state progressed = do
  (state', p', stepped) <- stepProcess p state
  case p' of
    Nothing -> tryStepProcesses ps done state' (progressed || stepped)
    Just p'' -> tryStepProcesses ps (p'' : done) state' (progressed || stepped)

-- | Step a single process
stepProcess :: Process -> NetState -> Either EvalError (NetState, Maybe Process, Bool)
stepProcess (Process pid cont) state = case cont of
  Done v ->
    -- Process completed, record result
    Right (state { netResults = Map.insert pid v (netResults state) }, Nothing, True)

  NewChan _ k -> do
    -- Create new channel
    let chanId = netNextChan state
        name = "chan_" ++ show chanId
        state' = state { netNextChan = chanId + 1
                       , netChannels = Map.insert name (ChanState []) (netChannels state) }
    Right (state', Just $ Process pid (k name), True)

  Send chanName val k -> do
    -- Try to send
    let chans = netChannels state
        chanState = fromMaybe (ChanState []) $ Map.lookup chanName chans
        chanState' = chanState { chanQueue = chanQueue chanState ++ [Right val] }
        state' = state { netChannels = Map.insert chanName chanState' chans }
    Right (state', Just $ Process pid k, True)

  Recv chanName k -> do
    -- Try to receive
    let chans = netChannels state
    case Map.lookup chanName chans of
      Just (ChanState (Right v : rest)) ->
        let state' = state { netChannels = Map.insert chanName (ChanState rest) chans }
        in Right (state', Just $ Process pid (k v), True)
      _ ->
        -- No message available, stay blocked
        Right (state, Just $ Process pid cont, False)

  SendLabel chanName lbl k -> do
    -- Send a label
    let chans = netChannels state
        chanState = fromMaybe (ChanState []) $ Map.lookup chanName chans
        chanState' = chanState { chanQueue = chanQueue chanState ++ [Left lbl] }
        state' = state { netChannels = Map.insert chanName chanState' chans }
    Right (state', Just $ Process pid k, True)

  RecvLabel chanName alts -> do
    -- Try to receive a label
    let chans = netChannels state
    case Map.lookup chanName chans of
      Just (ChanState (Left lbl : rest)) ->
        case lookup lbl alts of
          Just k ->
            let state' = state { netChannels = Map.insert chanName (ChanState rest) chans }
            in Right (state', Just $ Process pid k, True)
          Nothing ->
            Left $ ChannelError $ "Unknown label received: " ++ lbl
      _ ->
        -- No label available, stay blocked
        Right (state, Just $ Process pid cont, False)

  Fork childCont parentCont -> do
    -- Fork a new process
    let childPid = netNextProc state
        childProc = Process childPid (fmap (const VUnit) childCont)
        state' = state { netNextProc = childPid + 1
                       , netProcesses = childProc : netProcesses state }
    Right (state', Just $ Process pid parentCont, True)

  Close chanName k -> do
    -- Close a channel (just remove it)
    let state' = state { netChannels = Map.delete chanName (netChannels state) }
    Right (state', Just $ Process pid k, True)
