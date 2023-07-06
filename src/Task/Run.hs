module Task.Run where

import Data.List (intersect, union)
import qualified Data.Store as Store
import Polysemy
import Polysemy.Error
import Polysemy.Log
import Polysemy.Mutate
import Polysemy.Supply
import Polysemy.Writer
import Task.Input
import Task.Observe
import Task.Syntax

---- Logs and Errors -----------------------------------------------------------

data Steps
  = DidStabilise Nat Nat
  | DidNotStabilise Nat Nat Nat
  | DidNormalise Text
  | DidBalance Text
  | DidStart Text
  | DidCalculateOptions (List (Input Abstract)) Text
  | DidFinish

instance Display Steps where
  display = \case
    DidNotStabilise d w o -> unwords ["Found", display o, "overlap(s) amongst", display d, "dirty and", display w, "watched reference(s)"]
    DidStabilise d w -> unwords ["Found no overlaps amongst", display d, "dirty and", display w, "watched reference(s)"]
    DidNormalise t -> unwords ["Normalised to:", display t]
    DidBalance t -> unwords ["Rebalanced to:", display t]
    DidStart t -> unwords ["Started with:", display t]
    DidCalculateOptions os t -> unwords ["Future options before normalising", display os, "for task", display t]
    DidFinish -> "Done!"

data NotApplicable
  = CouldNotMatch Name Name
  | CouldNotChangeVal SomeTypeRep SomeTypeRep
  | CouldNotChangeRef SomeTypeRep SomeTypeRep
  | CouldNotGoTo Label
  | CouldNotFind Label
  | CouldNotPick
  | CouldNotContinue
  | CouldNotHandle (Input Concrete)
  | CouldNotHandleValue Concrete

instance Display NotApplicable where
  display = \case
    CouldNotMatch n m -> unwords ["Could not match editor id", display n |> quote, "with id", display m |> quote]
    CouldNotChangeVal b c -> unwords ["Could not change value because types", display b |> quote, "and", display c |> quote, "do not match"]
    CouldNotChangeRef r c -> unwords ["Could not change value because cell", display r |> quote, "does not contain", display c |> quote]
    CouldNotGoTo l -> unwords ["Could not pick label", display l |> quote, "because it leads to an empty task"]
    CouldNotFind l -> unwords ["Could not find label", display l |> quote, "in the possible options"]
    CouldNotPick -> unwords ["Could not pick because there is nothing to pick from in this task"]
    CouldNotContinue -> unwords ["Could not continue because there is no value to continue with"]
    CouldNotHandle i -> unwords ["Could not handle input", display i |> quote]
    CouldNotHandleValue b -> unwords ["Could not handle new editor value", display b |> quote, "for readonly editor"]

---- Normalising ---------------------------------------------------------------

normalise ::
  (Members '[Log Steps, Supply Nat, Alloc h, Read h, Write h] r) =>
  Task h a ->
  Sem (Writer (List (Some (Ref h))) ': r) (Task h a) -- NOTE: Here we're constructing a concrete stack. I don't know if that's the best way to go...
normalise t = case t of
  ---- Step
  Step t1 e2 -> do
    t1' <- normalise t1
    let stay = Step t1' e2
    mv1 <- raise <| value t1'
    case mv1 of
      Nothing -> done stay -- N-StepNone
      Just v1 -> do
        let t2 = e2 v1
        if failing t2
          then done stay -- N-StepFail
          else normalise t2 -- N-StepCont

  ---- Choose
  Choose t1 t2 -> do
    t1' <- normalise t1
    mv1 <- raise <| value t1'
    case mv1 of
      Just _ -> done t1' -- N-ChooseLeft
      Nothing -> do
        t2' <- normalise t2
        mv2 <- raise <| value t2'
        case mv2 of
          Just _ -> done t2' -- N-ChooseRight
          Nothing -> done <| Choose t1' t2' -- N-ChooseNone

  ---- Select
  Select Unnamed t1 es -> do
    k <- supply
    t1' <- normalise t1
    done <| Select (Named k) t1' es
  Select (Named k) t1 es -> do
    t1' <- normalise t1
    done <| Select (Named k) t1' es

  ---- Pool
  Pool Unnamed t0 ts -> do
    k <- supply
    t0' <- normalise t0
    ts' <- traverse normalise ts
    done <| Pool (Named k) t0' ts'
  Pool (Named k) t0 ts -> do
    t0' <- normalise t0
    ts' <- traverse normalise ts
    done <| Pool (Named k) t0' ts'

  ---- Converge
  Trans f t2 -> done (Trans f) -<< normalise t2
  Pair t1 t2 -> done Pair -<< normalise t1 -<< normalise t2
  ---- Ready
  Lift _ -> done t
  Fail -> done t
  ---- Name
  Edit Unnamed e -> do
    k <- supply
    done <| Edit (Named k) e
  Edit (Named _) _ -> done t
  ---- Resolve
  Assert p -> do
    done <| Lift p
  Share b -> do
    l <- Store.alloc b
    done <| Lift l
  Assign b s@(Store _ r) -> do
    Store.write b s
    tell [pack r]
    done <| Lift ()

-- balance :: Task h a -> Task h a
-- balance t = case t of
--   Pair t1 t2 -> Pair (balance t1) (balance t2)
--   Choose t1 t2 -> Choose (balance t1) (balance t2)
--   Trans f t1 -> Trans f (balance t1)
--   Forever t1 -> Forever (balance t1)
--   -- Monad associativity
--   Step (Step t1 e2) e3 -> Step t1 (\x -> Step (e2 x) e3)
--   _ -> t

-- balance' :: Editor m a -> Editor m a
-- balance' = \case
--   Select ts -> Select <| map balance ts
--   e -> e

---- Handling ------------------------------------------------------------------

handle ::
  forall h r a.
  (Members '[Alloc h, Read h, Write h] r) =>
  Task h a ->
  Input Concrete ->
  Sem (Writer (List (Some (Ref h))) ': Error NotApplicable ': r) (Task h a)
handle t i = case t of
  ---- Selections
  Select n t1 cs -> case i of
    Decide k l ->
      if n == Named k
        then do
          mv1 <- value t1
          case mv1 of
            Nothing -> throw <| CouldNotContinue
            Just v1 -> case lookup l cs of
              Nothing -> throw <| CouldNotFind l
              Just el ->
                let tl = el v1
                 in if failing tl
                      then throw <| CouldNotGoTo l
                      else done <| tl
        else do
          t1' <- handle t1 i
          done <| Select n t1' cs
    Insert _ _ -> do
      t1' <- handle t1 i
      done <| Select n t1' cs

  ---- Editors
  Edit n e -> case i of
    Insert k b' ->
      if n == Named k
        then do
          e' <- insert e b'
          done <| Edit n e' -- H-Edit
        else throw <| CouldNotMatch n (Named k)
    Decide _ _ -> throw <| CouldNotHandle i
  ---- Pass
  Trans e1 t2 -> do
    t2' <- handle t2 i
    done <| Trans e1 t2'
  Step t1 e2 -> do
    et1' <- try <| handle t1 i
    case et1' of
      Right t1' -> done <| Step t1' e2 -- H-Step
      Left x -> throw x
  Pair t1 t2 -> do
    et1' <- try <| handle t1 i
    case et1' of
      Right t1' -> done <| Pair t1' t2 -- H-PairFirst
      Left _ -> do
        t2' <- handle t2 i
        done <| Pair t1 t2' -- H-PairSecond
  Choose t1 t2 -> do
    et1' <- try <| handle t1 i
    case et1' of
      Right t1' -> done <| Choose t1' t2 -- H-ChooseFirst
      Left _ -> do
        t2' <- handle t2 i
        done <| Choose t1 t2' -- H-ChoosSecond

  ---- Rest
  _ -> throw <| CouldNotHandle i

insert ::
  forall h r a.
  (Members '[Read h, Write h] r) =>
  Editor h a -> -- NOTE: `Select` does not return an `Editor`...
  Concrete ->
  Sem (Writer (List (Some (Ref h))) ': Error NotApplicable ': r) (Editor h a)
insert e c@(Concrete b') = case e of
  Enter
    | Just Refl <- b' ~: beta -> done <| Update b'
    | otherwise -> throw <| CouldNotChangeVal (SomeTypeRep beta) (someTypeOf b')
    where
      beta = typeRep :: TypeRep a
  Update b
    -- NOTE: Here we check if `b` and `b'` have the same type.
    -- If this is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Update`.
    | Just Refl <- b ~= b' -> done <| Update b'
    | otherwise -> throw <| CouldNotChangeVal (someTypeOf b) (someTypeOf b')
  Change s@(Store _ r)
    -- NOTE: As in the `Update` case above, we check for type equality.
    | Just Refl <- b' ~: beta -> do
        Store.write b' s
        tell [pack r]
        done <| Change s
    | otherwise -> throw <| CouldNotChangeRef (someTypeOf r) (someTypeOf b')
    where
      beta = typeRep :: TypeRep a
  ---- Rest
  _ -> throw <| CouldNotHandleValue c

---- Fixation ------------------------------------------------------------------

fixate ::
  (Members '[Log Steps, Supply Nat, Alloc h, Read h, Write h] r) =>
  Sem (Writer (List (Some (Ref h))) ': r) (Task h a) ->
  Sem r (Task h a)
fixate t = do
  (d, t') <- runWriter t
  (d', t'') <- normalise t' |> runWriter
  log Info <| DidNormalise (display t'')
  let ws = watching t''
  let ds = d `union` d'
  let os = ds `intersect` ws
  case os of
    [] -> do
      log Info <| DidStabilise (length ds) (length ws)
      -- let t''' = balance t''
      -- log Info <| DidBalance (display t''')
      done t'' -- F-Lift
    _ -> do
      log Info <| DidNotStabilise (length ds) (length ws) (length os)
      fixate <| done t'' -- F-Loop

---- Initialisation ------------------------------------------------------------

initialise ::
  (Members '[Log Steps, Supply Nat, Alloc h, Read h, Write h] r) =>
  Task h a ->
  Sem r (Task h a)
initialise t = do
  log Info <| DidStart (display t)
  fixate (done t)

---- Interaction ---------------------------------------------------------------

interact ::
  (Members '[Log Steps, Log NotApplicable, Supply Nat, Alloc h, Read h, Write h] r) =>
  Input Concrete ->
  Task h a ->
  Sem r (Task h a)
interact i t = do
  xt <- handle t i |> runWriter |> runError
  case xt of
    Left e -> do
      log Warning e
      done t
    Right (_, t') -> fixate <| done t' -- XXX: forget delta?!

{-
execute ::
  Basic a =>
  List (Input Concrete) ->
  Task h a ->
  IO ()
execute events task = initialise task >>= go events
  where
    go events' task' = case events' of
      event : rest -> do
        task'' <- interact task' event
        go rest task''
      [] -> do
        result <- value task'
        putTextLn <| display result
-}
