module Task.Script.Checker
  ( check,
    match,
    execute,
    executeVerbose,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Task.Script.Syntax

---- Errors --------------------------------------------------------------------

data TypeError
  = UnknownVariable Name
  | ArgumentMismatch Ty Ty
  | BranchMismatch Ty Ty
  | AssignMismatch Ty Ty
  | FunctionNeeded Ty
  | BoolNeeded Ty
  | RecordNeeded Ty
  | ReferenceNeeded Ty
  | TaskNeeded Ty
  | BasicNeeded Ty
  | DoubleField (HashSet Label) (Row Ty)
  | EmptyChoice
  | HoleFound Context
  | MatchError MatchError
  deriving (Debug)

instance Display TypeError where
  display = \case
    UnknownVariable x -> unwords ["Unknown variable", x |> quote]
    -- VariableMismatch x t_exp t_act -> unwords ["Variable", quote x, "has type", display t_act, ", but it is expected to be a(n)", display t_exp]
    -- ConstantMismatch c t_exp t_act -> unwords ["Constant", quote (display c), "has type", display t_act, "but it is expected to be a(n)", display t_exp]
    ArgumentMismatch t_exp t_act -> unlines ["This function needs it argument to be of type", display t_exp |> quote |> indent 2, ", but it is of type", display t_act |> quote |> indent 2]
    BranchMismatch t_then t_else -> unlines ["This conditional's then-branch has type", display t_then |> quote |> indent 2, ", while the else-branch has type", display t_else |> quote |> indent 2]
    AssignMismatch t_ref t_val -> unlines ["This assignment tries to store something of type", display t_val |> quote |> indent 2, "into a reference of type", display t_ref |> quote |> indent 2]
    FunctionNeeded t_bad -> unwords ["Cannot use", display t_bad |> quote, "as a function"]
    -- BindNeeded t_bad -> unwords ["Cannot use", display t_bad |> quote, "as a a function from row to task"]
    RecordNeeded t_bad -> unwords ["Cannot use", display t_bad |> quote, "as a record"]
    BoolNeeded t_bad -> unwords ["Cannot use", display t_bad |> quote, "as a boolean"]
    ReferenceNeeded t_bad -> unwords ["Cannot use", display t_bad |> quote, "as a reference"]
    TaskNeeded t_bad -> unwords ["Cannot use", display t_bad |> quote, "as a task"]
    BasicNeeded t_bad -> unwords ["Cannot use", display t_bad |> quote, "as a basic type"]
    DoubleField r_double r_orig -> unwords ["Double occurense of label", display r_double |> quote, "in row", display r_orig |> quote]
    EmptyChoice -> unwords ["Choice is empty"]
    HoleFound g -> unlines ["Found hole of type _ in context", display g |> indent 2]
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

class Check a where
  check :: Members '[Error TypeError, Error MatchError, Output Text] r => Context -> a -> Sem r Ty

instance Check Expression where
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
        TFunction t' t -> do
          t2 <- check g e2
          if t' == t2
            then pure t
            else throw <| ArgumentMismatch t' t2
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

instance Check Argument where
  check g (ARecord es) = traverse (check g) es ||> TRecord

instance Check Statement where
  check g = \case
    Step m t s -> do
      t_t <- check g t
      case t_t of
        TTask r -> do
          d <- match m (TRecord r)
          check (g \/ d) s
        _ -> throw <| TaskNeeded t_t
    Task t -> check g t

instance Check Task where
  check g = \case
    Enter b _ -> ofBasic b |> returnValue
    Update _ e -> check g e ||= needBasic ||= returnValue
    Change _ e -> check g e ||= outofReference ||= returnValue
    View _ e -> check g e ||= needBasic ||= returnValue
    Watch _ e -> check g e ||= outofReference ||= returnValue
    Done e -> check g e ||= outofRecord ||> TTask
    Pair ss -> traverse go ss ||= unite ||> TTask
    Choose ss -> traverse go ss ||= intersect ||> TTask
    Branch bs -> traverse go' bs ||= intersect ||> TTask
    Select bs -> traverse go'' bs ||= intersect ||> TTask
    Execute x a -> do
      t_x <- HashMap.lookup x g |> note (UnknownVariable x)
      case t_x of
        TFunction r' t -> do
          t_a <- check g a
          if r' == t_a
            then pure t
            else throw <| ArgumentMismatch r' t_a
        _ -> throw <| FunctionNeeded t_x
    Hole _ -> throw <| HoleFound g --TODO: how to handle holes?
    Share e -> check g e ||= outofBasic ||> TReference ||= returnValue
    Assign e1 e2 -> do
      t1 <- check g e1
      b1 <- outofReference t1
      b2 <- check g e2
      if b1 == b2
        then pure (TRecord [])
        else throw (AssignMismatch b1 b2)
    where
      go s = check g s ||= outofTask
      go' (e, s) = do
        t_e <- check g e
        case t_e of
          TPrimitive TBool -> go s
          _ -> throw <| BoolNeeded t_e
      go'' (_, e, s) = go' (e, s)

-- go f acc s = check g s ||= outofTask ||> f acc
-- go' f acc (e, s) = do
--   t_e <- check g e
--   case t_e of
--     TPrimitive TBool -> go f acc s
--     _ -> throw <| BoolNeeded t_e
-- go'' f acc (_, e, s) = go' f acc (e, s)

unite :: (Members '[Error TypeError] r) => List (Row Ty) -> Sem r (Row Ty)
unite rs = go [] rs
  where
    go :: (Members '[Error TypeError] r) => Row Ty -> List (Row Ty) -> Sem r (Row Ty)
    go acc = \case
      [] -> pure acc
      r : rest ->
        let d = HashMap.keysSet r `HashSet.intersection` HashMap.keysSet acc
         in if null d
              then go (acc \/ r) rest
              else throw <| DoubleField d r

intersect :: (Members '[Error TypeError] r) => List (Row a) -> Sem r (Row a)
intersect = \case
  [] -> throw <| EmptyChoice
  r : rs -> go r rs
  where
    go acc = \case
      [] -> pure acc
      r : rest -> go (acc /\ r) rest

needBasic :: (Members '[Error TypeError] r) => Ty -> Sem r Ty
needBasic t
  | isBasic t = pure t
  | otherwise = throw <| BasicNeeded t

outofBasic :: (Members '[Error TypeError] r) => Ty -> Sem r BasicTy
outofBasic t
  | Just b <- ofType t = pure b
  | otherwise = throw <| BasicNeeded t

outofRecord :: (Members '[Error TypeError] r) => Ty -> Sem r (Row Ty)
outofRecord t
  | Just r <- ofRecord t = pure r
  | otherwise = throw <| RecordNeeded t

outofReference :: (Members '[Error TypeError] r) => Ty -> Sem r Ty
outofReference t
  | Just b <- ofReference t = pure <| ofBasic b
  | otherwise = throw <| ReferenceNeeded t

outofTask :: (Members '[Error TypeError] r) => Ty -> Sem r (Row Ty)
outofTask t
  | Just r <- ofTask t = pure r
  | otherwise = throw <| TaskNeeded t

returnValue :: (Monad m) => Ty -> m Ty
returnValue t = pure <| TTask ["value" ~> t]

---- Matcher -------------------------------------------------------------------

match :: (Members '[Error MatchError, Output Text] r) => Match -> Ty -> Sem r Context
match m t = case m of
  MIgnore -> pure []
  MBind x -> pure [x ~> t]
  MRecord ms -> do
    case t of
      TRecord r -> HashMap.intersectionWith (,) ms r |> gather go []
      _ -> throw <| RecordMismatch ms t
    where
      go acc (mx, tx) = match mx tx ||> (\/) acc
  MUnpack -> do
    case t of
      TRecord r -> pure r
      _ -> throw <| UnpackMismatch t

---- Helpers -------------------------------------------------------------------

execute :: Sem '[Error MatchError, Error TypeError, Output o] a -> Either TypeError a
execute = mapError MatchError >> runError >> ignoreOutput >> run

executeVerbose :: Sem '[Error MatchError, Error TypeError, Output o] a -> (List o, Either TypeError a)
executeVerbose = mapError MatchError >> runError >> runOutputList >> run

(\/) :: (Hash k) => HashMap k v -> HashMap k v -> HashMap k v
(\/) = HashMap.union

(/\) :: (Hash k) => HashMap k v -> HashMap k w -> HashMap k v
(/\) = HashMap.intersection
