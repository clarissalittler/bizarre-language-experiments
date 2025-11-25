-- | Pretty printing for NuPi terms and types
module NuPi.Pretty
  ( prettyTy
  , prettyTerm
  , prettyValue
  ) where

import NuPi.Syntax

import Data.List (intercalate)

-- | Pretty print a type
prettyTy :: Ty -> String
prettyTy TyUnit = "1"
prettyTy TyInt = "Int"
prettyTy TyBool = "Bool"
prettyTy (TyProd a b) = "(" ++ prettyTy a ++ " × " ++ prettyTy b ++ ")"
prettyTy (TyArrow a b) = "(" ++ prettyTy a ++ " → " ++ prettyTy b ++ ")"
prettyTy (TyLollipop a b) = "(" ++ prettyTy a ++ " ⊸ " ++ prettyTy b ++ ")"
prettyTy (TyTensor a b) = "(" ++ prettyTy a ++ " ⊗ " ++ prettyTy b ++ ")"
prettyTy (TyPlus alts) = "(⊕{" ++ intercalate ", " [l ++ ": " ++ prettyTy t | (l, t) <- alts] ++ "})"
prettyTy (TyWith alts) = "(&{" ++ intercalate ", " [l ++ ": " ++ prettyTy t | (l, t) <- alts] ++ "})"
prettyTy (TyMu x a) = "(μ" ++ x ++ "." ++ prettyTy a ++ ")"
prettyTy (TyNu x a) = "(ν" ++ x ++ "." ++ prettyTy a ++ ")"
prettyTy (TyVar x) = x
prettyTy (TyChan a) = "Chan(" ++ prettyTy a ++ ")"
prettyTy (TyProc a) = "Proc(" ++ prettyTy a ++ ")"

-- | Pretty print a term
prettyTerm :: Term -> String
prettyTerm (TmVar x) = x
prettyTerm TmUnit = "()"
prettyTerm (TmInt n) = show n
prettyTerm (TmBool b) = if b then "true" else "false"
prettyTerm (TmPair e1 e2) = "(" ++ prettyTerm e1 ++ ", " ++ prettyTerm e2 ++ ")"
prettyTerm (TmFst e) = "fst " ++ prettyTerm e
prettyTerm (TmSnd e) = "snd " ++ prettyTerm e
prettyTerm (TmLam x ty e) = "(λ" ++ x ++ ":" ++ prettyTy ty ++ ". " ++ prettyTerm e ++ ")"
prettyTerm (TmLinLam x ty e) = "(λ!" ++ x ++ ":" ++ prettyTy ty ++ ". " ++ prettyTerm e ++ ")"
prettyTerm (TmApp e1 e2) = "(" ++ prettyTerm e1 ++ " " ++ prettyTerm e2 ++ ")"
prettyTerm (TmTensor e1 e2) = "(" ++ prettyTerm e1 ++ " ⊗ " ++ prettyTerm e2 ++ ")"
prettyTerm (TmLetTensor x y e1 e2) =
  "let (" ++ x ++ " ⊗ " ++ y ++ ") = " ++ prettyTerm e1 ++ " in " ++ prettyTerm e2
prettyTerm (TmInj lbl e _) = "inj[" ++ lbl ++ "] " ++ prettyTerm e
prettyTerm (TmCase e alts) =
  "case " ++ prettyTerm e ++ " of {" ++
  intercalate "; " [lbl ++ " " ++ x ++ " → " ++ prettyTerm t | (lbl, x, t) <- alts] ++ "}"
prettyTerm (TmRecord fields) =
  "{" ++ intercalate ", " [lbl ++ " = " ++ prettyTerm e | (lbl, e) <- fields] ++ "}"
prettyTerm (TmSelect lbl e) = prettyTerm e ++ "." ++ lbl
prettyTerm (TmFold ty e) = "fold[" ++ prettyTy ty ++ "] " ++ prettyTerm e
prettyTerm (TmUnfold e) = "unfold " ++ prettyTerm e
prettyTerm (TmCoiter ty seed step) =
  "coiter[" ++ prettyTy ty ++ "](" ++ prettyTerm seed ++ ", " ++ prettyTerm step ++ ")"
prettyTerm (TmUnfoldNu e) = "unfoldNu " ++ prettyTerm e
prettyTerm (TmFix x ty e) = "fix " ++ x ++ ":" ++ prettyTy ty ++ ". " ++ prettyTerm e
prettyTerm (TmNewChan ty) = "newChan[" ++ prettyTy ty ++ "]"
prettyTerm (TmSend c v) = "send " ++ prettyTerm c ++ " " ++ prettyTerm v
prettyTerm (TmRecv c) = "recv " ++ prettyTerm c
prettyTerm (TmClose c) = "close " ++ prettyTerm c
prettyTerm (TmFork e) = "fork " ++ prettyTerm e
prettyTerm (TmReturn e) = "return " ++ prettyTerm e
prettyTerm (TmBind x e1 e2) = "do " ++ x ++ " <- " ++ prettyTerm e1 ++ "; " ++ prettyTerm e2
prettyTerm (TmSendLabel lbl c) = "sendLabel[" ++ lbl ++ "] " ++ prettyTerm c
prettyTerm (TmRecvLabel c alts) =
  "recvLabel " ++ prettyTerm c ++ " {" ++
  intercalate "; " [lbl ++ " → " ++ prettyTerm t | (lbl, t) <- alts] ++ "}"
prettyTerm (TmLet x e1 e2) = "let " ++ x ++ " = " ++ prettyTerm e1 ++ " in " ++ prettyTerm e2

-- | Pretty print a value
prettyValue :: Value -> String
prettyValue VUnit = "()"
prettyValue (VInt n) = show n
prettyValue (VBool b) = if b then "true" else "false"
prettyValue (VPair v1 v2) = "(" ++ prettyValue v1 ++ ", " ++ prettyValue v2 ++ ")"
prettyValue (VTensor v1 v2) = "(" ++ prettyValue v1 ++ " ⊗ " ++ prettyValue v2 ++ ")"
prettyValue (VClosure x _ _) = "<closure:" ++ x ++ ">"
prettyValue (VLinClosure x _ _) = "<lin-closure:" ++ x ++ ">"
prettyValue (VInj lbl v) = "inj[" ++ lbl ++ "] " ++ prettyValue v
prettyValue (VRecord fields) =
  "{" ++ intercalate ", " [lbl ++ " = " ++ prettyValue v | (lbl, v) <- fields] ++ "}"
prettyValue (VFold v) = "fold " ++ prettyValue v
prettyValue (VCoiter seed step) = "<coiter: " ++ prettyValue seed ++ ", " ++ prettyValue step ++ ">"
prettyValue (VChan name end) = "<chan:" ++ name ++ ":" ++ show end ++ ">"
prettyValue (VProc k) = "<proc: " ++ show k ++ ">"
