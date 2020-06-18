module Task.Script.Checker where

import qualified Data.HashMap.Strict as HashMap
import Polysemy
import Polysemy.Error
import Task.Script.Syntax

---- Errors --------------------------------------------------------------------

data TypeError
  = UnknownVariable Name
  | ArgumentMismatch Ty Ty
  | BranchMismatch Ty Ty
  | FunctionNeeded Ty
  | BoolNeeded Ty
  | MatchError MatchError
  deriving (Debug)

-- VariableMismatch Name {-used-} Ty {-actual-} Ty
-- ConstantMismatch Constant {-used-} Ty {-actual-} Ty

instance Display TypeError where
  display = \case
    UnknownVariable x -> unwords ["Unknown variable", quote x]
    -- VariableMismatch x t_exp t_act -> unwords ["Variable", quote x, "has type", display t_act, ", but it is expected to be a(n)", display t_exp]
    -- ConstantMismatch c t_exp t_act -> unwords ["Constant", quote (display c), "has type", display t_act, "but it is expected to be a(n)", display t_exp]
    ArgumentMismatch t_exp t_act -> unwords ["This function needs it argument to be of type", quote (display t_exp), ", but it is of type", quote (display t_act)]
    BranchMismatch t_then t_else -> unwords ["The then-branch of this conditional has type", quote (display t_then), ", while the else-branch has type", quote (display t_else)]
    FunctionNeeded t_bad -> unwords ["Cannot use", quote (display t_bad), "as a function"]
    BoolNeeded t_bad -> unwords ["Cannot test", quote (display t_bad), "for truth"]
    MatchError e -> display e

data MatchError
  = RecordMismatch (Row Match) Ty
  | UnpackMismatch Ty
  deriving (Debug)

instance Display MatchError where
  display = \case
    RecordMismatch ms t -> unwords ["Matching against", display ms, "needs", display t, "to be a record type"]
    UnpackMismatch t -> unwords ["Unpacking needs", display t, "to be a record type"]

---- Checker -------------------------------------------------------------------

type Context = HashMap Name Ty

check :: Members '[Error TypeError, Error MatchError] r => Context -> Expression -> Sem r Ty
check g = \case
  ---- Basics
  Variable x -> HashMap.lookup x g |> note (UnknownVariable x)
  Lambda m t e -> do
    d <- match m t
    t' <- check (g \/ d) e
    pure <| TFunction t t'
  Apply e1 e2 -> do
    t1 <- check g e1
    case t1 of
      TFunction t t' -> do
        t2 <- check g e2
        if t == t2
          then pure t'
          else throw <| ArgumentMismatch t t2
      _ -> throw <| FunctionNeeded t1
  ---- Branches
  IfThenElse e1 e2 e3 -> do
    t1 <- check g e1
    case t1 of
      TPrimitive TBool -> do
        t2 <- check g e2
        t3 <- check g e3
        if t2 == t3
          then pure t2
          else throw <| BranchMismatch t2 t3
      _ -> throw <| BoolNeeded t1
  ---- Records
  Record es -> traverse (check g) es ||> TRecord
  ---- Constants
  Constant (B _) -> pure <| TPrimitive TBool
  Constant (I _) -> pure <| TPrimitive TInt
  Constant (S _) -> pure <| TPrimitive TString

match :: Members '[Error MatchError] r => Match -> Ty -> Sem r Context
match m t = case m of
  MIgnore -> pure []
  MBind x -> pure [x ~> t]
  MRecord ms -> do
    case t of
      TRecord r -> HashMap.intersectionWith (,) ms r |> foldlM go []
      _ -> throw <| RecordMismatch ms t
    where
      go acc (mx, tx) = match mx tx ||> HashMap.intersection acc
  MUnpack -> do
    case t of
      TRecord r -> pure r
      _ -> throw <| UnpackMismatch t

execute :: Sem '[Error MatchError, Error TypeError] a -> Either TypeError a
execute = mapError MatchError >> runError >> run

---- Helpers -------------------------------------------------------------------

(\/) :: (Hash k) => HashMap k v -> HashMap k v -> HashMap k v
(\/) = HashMap.union

(/\) :: (Hash k) => HashMap k v -> HashMap k w -> HashMap k v
(/\) = HashMap.intersection
