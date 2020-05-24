module Data.Task.Interact where

import qualified Data.HashMap.Strict as HashMap
import Data.Heap (Ref)
import Data.List (intersect, union)
import qualified Data.Store as Store
import Data.Task
import Data.Task.Input
import Data.Task.Observe
import qualified Data.Text.Prettyprint.Doc as Pretty
import Polysemy
import Polysemy.Error
import Polysemy.Log
import Polysemy.Mutate
import Polysemy.Output
import Polysemy.Supply
import Polysemy.Writer

-- Logs and Errors -------------------------------------------------------------

data Steps
  = DidStabilise Nat Nat
  | DidNotStabilise Nat Nat Nat
  | DidNormalise Text
  | DidBalance Text
  | DidStart Text
  | DidCalculateOptions (List Option) Text

instance Pretty Steps where
  pretty = \case
    DidNotStabilise d w o -> sep ["Found", pretty o, "overlap(s) amongst", pretty d, "dirty and", pretty w, "watched reference(s)"]
    DidStabilise d w -> sep ["Found no overlaps amongst", pretty d, "dirty and", pretty w, "watched reference(s)"]
    DidNormalise t -> sep ["Normalised to:", pretty t]
    DidBalance t -> sep ["Rebalanced to:", pretty t]
    DidStart t -> sep ["Started with:", pretty t]
    DidCalculateOptions os t -> sep ["Future options before normalising", pretty os, "for task", pretty t]

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

instance Pretty NotApplicable where
  pretty = \case
    CouldNotMatch n m -> sep ["Could not match editor id", Pretty.dquotes <| pretty n, "with id", Pretty.dquotes <| pretty m]
    CouldNotChangeVal b c -> sep ["Could not change value because types", Pretty.dquotes <| pretty b, "and", Pretty.dquotes <| pretty c, "do not match"]
    CouldNotChangeRef r c -> sep ["Could not change value because cell", Pretty.dquotes <| pretty r, "does not contain", Pretty.dquotes <| pretty c]
    CouldNotGoTo l -> sep ["Could not pick label", Pretty.dquotes <| pretty l, "because it leads to an empty task"]
    CouldNotFind l -> sep ["Could not find label", Pretty.dquotes <| pretty l, "in the possible options"]
    CouldNotSelect -> sep ["Could not pick because there is nothing to pick from in this task"]
    CouldNotContinue -> sep ["Could not continue because there is no value to continue with"]
    CouldNotHandle i -> sep ["Could not handle input", Pretty.dquotes <| pretty i]
    CouldNotHandleValue b -> sep ["Could not handle new editor value", Pretty.dquotes <| pretty b, "for readonly editor"]

-- Normalising -----------------------------------------------------------------

normalise ::
  Members '[Log Steps, Supply Nat, Writer (List (Someref h)), Alloc h, Read h, Write h] r =>
  Task h (Sem r) a ->
  Sem r (Task h (Sem r) a)
normalise t = case t of
  -- Step --
  Step t1 e2 -> do
    t1' <- normalise t1
    let stay = Step t1' e2
    mv1 <- value t1'
    case mv1 of
      Nothing -> pure stay -- N-StepNone
      Just v1 -> do
        let t2 = e2 v1
        if failing t2
          then pure stay -- N-StepFail
          else do
            let os = options t2
            log Info <| DidCalculateOptions os (pretty t2 |> show)
            if not <| null <| os
              then pure stay -- N-StepWait
              else normalise t2 -- N-StepCont

  -- Choose --
  Choose t1 t2 -> do
    t1' <- normalise t1
    mv1 <- value t1'
    case mv1 of
      Just _ -> pure t1' -- N-ChooseLeft
      Nothing -> do
        t2' <- normalise t2
        mv2 <- value t2'
        case mv2 of
          Just _ -> pure t2' -- N-ChooseRight
          Nothing -> pure <| Choose t1' t2' -- N-ChooseNone

  -- Congruences --
  Trans f t2 -> pure (Trans f) -< normalise t2
  Pair t1 t2 -> pure Pair -< normalise t1 -< normalise t2
  -- Ready --
  Done _ -> pure t
  Fail -> pure t
  -- Editors --
  Editor Unnamed e -> do
    n <- supply
    pure <| Editor (Named n) e
  Editor (Named _) _ -> pure t
  -- Assert --
  -- Assert p -> do
  --   pure <| Done p
  -- References --
  Share b -> do
    l <- Store.alloc b
    pure <| Done l
  Assign b s@(Store _ r) -> do
    Store.write b s
    tell [pack r]
    pure <| Done ()

-- balance :: Task h m a -> Task h m a
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

-- Handling --------------------------------------------------------------------

-- type PartialTracking f m a = WriterT (List (Someref m)) (ExceptT NotApplicable m) (f m a)

handle ::
  forall h r a.
  Members '[Error NotApplicable, Writer (List (Someref h)), Alloc h, Read h, Write h] r =>
  Task h (Sem r) a ->
  Input Concrete ->
  Sem r (Task h (Sem r) a)
handle t i = case t of
  -- Editors --
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
  -- Pass --
  Trans e1 t2 -> do
    t2' <- handle t2 i
    pure <| Trans e1 t2'
  Step t1 e2 -> do
    et1' <- try <| handle t1 i
    case et1' of
      Right t1' -> pure <| Step t1' e2 -- H-Step
      Left _ -> do
        mv1 <- value t1
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
            -- Rest --
  _ -> throw <| CouldNotHandle i

handle' ::
  forall h m r a.
  Members '[Error NotApplicable, Writer (List (Someref h)), Read h, Write h] r =>
  Concrete ->
  Editor h m a -> -- NOTE: `Select` does not return an `Editor`...
  Sem r (Editor h m a)
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
  -- Rest --
  _ -> throw <| CouldNotHandleValue c

-- Fixation --------------------------------------------------------------------

fixate ::
  Members '[Log Steps, Supply Nat, Writer (List (Someref h)), Alloc h, Read h, Write h] r =>
  Sem r (Task h (Sem r) a) ->
  Sem r (Task h (Sem r) a)
fixate t = do
  (d, t') <- listen t --FIXME: Is this correct??
  (d', t'') <- listen <| normalise t' --FIXME: Is this correct??
  log Info <| DidNormalise (show <| pretty t'')
  let ws = watching t''
  let ds = d `union` d'
  let os = ds `intersect` ws
  case os of
    [] -> do
      log Info <| DidStabilise (length ds) (length ws)
      -- let t''' = balance t''
      -- log Info <| DidBalance (show <| pretty t''')
      pure t'' -- F-Done --
    _ -> do
      log Info <| DidNotStabilise (length ds) (length ws) (length os)
      fixate <| pure t'' -- F-Loop --

-- Initialisation --------------------------------------------------------------

initialise ::
  Members '[Log Steps, Supply Nat, Writer (List (Someref h)), Alloc h, Read h, Write h] r =>
  Task h (Sem r) a ->
  Sem r (Task h (Sem r) a)
initialise t = do
  log Info <| DidStart (show <| pretty t)
  fixate (pure t)

-- Interaction -----------------------------------------------------------------

{-
interact ::
  Members '[Log Steps, Writer (List (Someref h)), Error NotApplicable, Alloc h, Read h, Write h] r =>
  Input Concrete ->
  Task h (Sem r) a ->
  Sem r (Task h (Sem r) a)
interact i t = do
  -- (_, t') <- listen <| handle t i
  x <- _ t
  case x of
    Left e -> do
      log Warning e
      pure t
    Right t'' -> fixate <| pure t''

execute ::
  Editable a =>
  List (Input Concrete) ->
  Task IO a ->
  IO ()
execute events task = initialise task >>= go events
  where
    go events' task' = case events' of
      event : rest -> do
        task'' <- interact task' event
        go rest task''
      [] -> do
        result <- value task'
        putTextLn <| show <| pretty result

-- Running ---------------------------------------------------------------------

getUserInput :: IO (Input Concrete)
getUserInput = do
  putText ">> "
  line <- getLine
  case line of
    "quit" -> exitSuccess
    "q" -> exitSuccess
    _ -> case parse line of
      Right input -> pure input
      Left message -> do
        print message
        getUserInput

run :: Task IO a -> IO ()
run task = do
  task' <- initialise task
  loop task'
  where
    loop :: Task IO a -> IO ()
    loop task' = do
      putTextLn ""
      interface <- ui task'
      print interface
      events <- inputs task'
      print <| "Possibilities: " ++ pretty events
      input <- getUserInput
      task'' <- interact task' input
      loop task''

-}
