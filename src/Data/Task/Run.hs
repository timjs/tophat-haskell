module Data.Task.Run where

import qualified Data.HashMap.Strict as HashMap
import Data.List (intersect, union)
import qualified Data.Store as Store
import Data.Task
import Data.Task.Input
import Data.Task.Observe
import Polysemy
import Polysemy.Error
import Polysemy.Log
import Polysemy.Mutate
import Polysemy.Supply
import Polysemy.Writer

---- Logs and Errors -----------------------------------------------------------

data Steps
  = DidStabilise Nat Nat
  | DidNotStabilise Nat Nat Nat
  | DidNormalise Text
  | DidBalance Text
  | DidStart Text
  | DidCalculateOptions (List Option) Text
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
  | CouldNotSelect
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
    CouldNotSelect -> unwords ["Could not pick because there is nothing to pick from in this task"]
    CouldNotContinue -> unwords ["Could not continue because there is no value to continue with"]
    CouldNotHandle i -> unwords ["Could not handle input", display i |> quote]
    CouldNotHandleValue b -> unwords ["Could not handle new editor value", display b |> quote, "for readonly editor"]

---- Normalising ---------------------------------------------------------------

normalise ::
  Members '[Log Steps, Supply Nat, Alloc h, Read h, Write h] r =>
  Task h a ->
  Sem (Writer (List (Some (Ref h))) ': r) (Task h a) --NOTE: Here we're constructing a concrete stack. I don't know if that's the best way to go...
normalise t = case t of
  ---- Step
  Step t1 e2 -> do
    t1' <- normalise t1
    let stay = Step t1' e2
    mv1 <- raise <| value t1'
    case mv1 of
      Nothing -> pure stay -- N-StepNone
      Just v1 -> do
        let t2 = e2 v1
        if failing t2
          then pure stay -- N-StepFail
          else do
            let os = options t2
            log Info <| DidCalculateOptions os (display t2)
            if not <| null <| os
              then pure stay -- N-StepWait
              else normalise t2 -- N-StepCont

  ---- Choose
  Choose t1 t2 -> do
    t1' <- normalise t1
    mv1 <- raise <| value t1'
    case mv1 of
      Just _ -> pure t1' -- N-ChooseLeft
      Nothing -> do
        t2' <- normalise t2
        mv2 <- raise <| value t2'
        case mv2 of
          Just _ -> pure t2' -- N-ChooseRight
          Nothing -> pure <| Choose t1' t2' -- N-ChooseNone

  ---- Congruences
  Trans f t2 -> pure (Trans f) -< normalise t2
  Pair t1 t2 -> pure Pair -< normalise t1 -< normalise t2
  ---- Ready
  Done _ -> pure t
  Fail -> pure t
  ---- Editors
  Editor Unnamed e -> do
    n <- supply
    pure <| Editor (Named n) e
  Editor (Named _) _ -> pure t
  ---- Checks
  Assert p -> do
    pure <| Done p
  ---- References
  Share b -> do
    l <- Store.alloc b --XXX: raise?
    pure <| Done l
  Assign b s@(Store _ r) -> do
    Store.write b s
    tell [pack r]
    pure <| Done ()

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
  Members '[Alloc h, Read h, Write h] r =>
  Task h a ->
  Input Concrete ->
  Sem (Writer (List (Some (Ref h))) ': Error NotApplicable ': r) (Task h a)
handle t i = case t of
  ---- Editors
  Editor n e -> case i of
    IOption n' l -> case e of
      Select ts
        | n == n' -> case HashMap.lookup l ts of
          Nothing -> throw <| CouldNotFind l
          Just t' -> do
            let os = options t
            if Option n l `elem` os
              then pure t'
              else throw <| CouldNotGoTo l
        | otherwise -> throw <| CouldNotMatch n n'
      _ -> throw <| CouldNotHandle i
    IEnter m b'
      | n == Named m -> do
        e' <- handle' b' e
        pure <| Editor n e'
      | otherwise -> throw <| CouldNotMatch n (Named m)
  ---- Pass
  Trans e1 t2 -> do
    t2' <- handle t2 i
    pure <| Trans e1 t2'
  Step t1 e2 -> do
    et1' <- try <| handle t1 i
    case et1' of
      Right t1' -> pure <| Step t1' e2 -- H-Step
      Left _ -> do
        mv1 <- raise <| raise <| value t1
        case mv1 of
          Nothing -> throw CouldNotContinue
          Just v1 -> do
            let t2 = e2 v1
            handle t2 i -- H-StepCont
  Pair t1 t2 -> do
    et1' <- try <| handle t1 i
    case et1' of
      Right t1' -> pure <| Pair t1' t2 -- H-PairFirst
      Left _ -> do
        t2' <- handle t2 i
        pure <| Pair t1 t2' -- H-PairSecond
  Choose t1 t2 -> do
    et1' <- try <| handle t1 i
    case et1' of
      Right t1' -> pure <| Choose t1' t2 -- H-ChooseFirst
      Left _ -> do
        t2' <- handle t2 i
        pure <| Choose t1 t2' -- H-ChoosSecond
        ---- Rest
  _ -> throw <| CouldNotHandle i

handle' ::
  forall h r a.
  Members '[Read h, Write h] r =>
  Concrete ->
  Editor h a -> -- NOTE: `Select` does not return an `Editor`...
  Sem (Writer (List (Some (Ref h))) ': Error NotApplicable ': r) (Editor h a)
handle' c@(Concrete b') = \case
  Enter
    | Just Refl <- b' ~: beta -> pure <| Update b'
    | otherwise -> throw <| CouldNotChangeVal (SomeTypeRep beta) (someTypeOf b')
    where
      beta = typeRep :: TypeRep a
  Update b
    -- NOTE: Here we check if `b` and `b'` have the same type.
    -- If this is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Update`.
    | Just Refl <- b ~= b' -> pure <| Update b'
    | otherwise -> throw <| CouldNotChangeVal (someTypeOf b) (someTypeOf b')
  Change s@(Store _ r)
    -- NOTE: As in the `Update` case above, we check for type equality.
    | Just Refl <- b' ~: beta -> do
      Store.write b' s
      tell [pack r]
      pure <| Change s
    | otherwise -> throw <| CouldNotChangeRef (someTypeOf r) (someTypeOf b')
    where
      beta = typeRep :: TypeRep a
  ---- Rest
  _ -> throw <| CouldNotHandleValue c

---- Fixation ------------------------------------------------------------------

fixate ::
  Members '[Log Steps, Supply Nat, Alloc h, Read h, Write h] r =>
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
      pure t'' -- F-Done
    _ -> do
      log Info <| DidNotStabilise (length ds) (length ws) (length os)
      fixate <| pure t'' -- F-Loop

---- Initialisation ------------------------------------------------------------

initialise ::
  Members '[Log Steps, Supply Nat, Alloc h, Read h, Write h] r =>
  Task h a ->
  Sem r (Task h a)
initialise t = do
  log Info <| DidStart (display t)
  fixate (pure t)

---- Interaction ---------------------------------------------------------------

interact ::
  Members '[Log Steps, Log NotApplicable, Supply Nat, Alloc h, Read h, Write h] r =>
  Input Concrete ->
  Task h a ->
  Sem r (Task h a)
interact i t = do
  xt <- handle t i |> runWriter |> runError
  case xt of
    Left e -> do
      log Warning e
      pure t
    Right (_, t') -> fixate <| pure t' --XXX: forget delta?!

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
