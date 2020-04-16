module Data.Task.Run where

import Control.Monad.Log
import Control.Monad.Supply
import qualified Data.HashMap.Strict as HashMap
import Data.List (intersect, union)
import Data.Task
import Data.Task.Input
import qualified Data.Text.Prettyprint.Doc as Pretty

-- Observations ----------------------------------------------------------------
-- NOTE: Normalisation should never happen in any observation, they are immediate.

ui ::
  Collaborative r m =>
  Task m a ->
  m (Doc n) -- We need to watch locations and be in monad `m`
ui = \case
  Editor a e -> ui' a e
  Done _ -> pure "■ …"
  Pair t1 t2 -> pure (\l r -> sep [l, " ⧓ ", r]) -< ui t1 -< ui t2
  Choose t1 t2 -> pure (\l r -> sep [l, " ◆ ", r]) -< ui t1 -< ui t2
  Fail -> pure "↯"
  Trans _ t -> ui t
  Step t1 e2 -> pure go -< ui t1 -< do
    mv1 <- value t1
    case mv1 of
      Nothing -> pure []
      Just v1 -> do
        os <- options (e2 v1)
        pure <| map (\(IOption _ l) -> l) os --FIXME
    where
      go s ls
        | null ls = cat [s, " ▶…"]
        | otherwise = cat [s, " ▷", pretty ls]
  Share b -> pure <| sep ["share", pretty b]
  Assign b _ -> pure <| sep ["…", ":=", pretty b]

ui' ::
  forall r m n b.
  Collaborative r m =>
  Name ->
  Editor m b ->
  m (Doc n) -- We need to watch locations and be in monad `m`
ui' a = \case
  Enter -> pure <| cat ["⊠^", pretty a, "_", pretty (typeRep :: TypeRep b)]
  Update b -> pure <| cat ["□^", pretty a, " [ ", pretty b, " ]"]
  View b -> pure <| cat ["⧇^", pretty a, " [ ", pretty b, " ]"]
  Select ts -> pure <| cat ["◇^", pretty a, " ", pretty <| HashMap.keysSet ts]
  Change l -> pure (\b -> cat ["⊟^", pretty a, " [ ", pretty b, " ]"]) -< watch l
  Watch l -> pure (\b -> cat ["⧈^", pretty a, " [ ", pretty b, " ]"]) -< watch l

value ::
  Collaborative r m =>
  Task m a ->
  m (Maybe a) -- We need to watch locations and be in monad `m`
value = \case
  Editor Unnamed _ -> pure Nothing
  Editor (Named _) e -> value' e
  Trans f t -> pure (map f) -< value t
  Pair t1 t2 -> pure (><) -< value t1 -< value t2
  Done b -> pure (Just b)
  Choose t1 t2 -> pure (<|>) -< value t1 -< value t2
  Fail -> pure Nothing
  Step _ _ -> pure Nothing
  Share b -> pure Just -< share b -- Nothing??
  Assign _ _ -> pure (Just ()) -- Nothing??

value' ::
  Collaborative r m =>
  Editor m a ->
  m (Maybe a) -- We need to watch locations and be in monad `m`
value' = \case
  Enter -> pure Nothing
  Update b -> pure (Just b)
  View b -> pure (Just b)
  Select _ -> pure Nothing
  Change l -> pure Just -< watch l
  Watch l -> pure Just -< watch l

failing ::
  Task m a -> Bool
failing = \case
  Editor _ e -> failing' e
  Trans _ t -> failing t
  Pair t1 t2 -> failing t1 && failing t2
  Done _ -> False
  Choose t1 t2 -> failing t1 && failing t2
  Fail -> True
  Step t _ -> failing t
  Share _ -> False
  Assign _ _ -> False

failing' ::
  Editor m a -> Bool
failing' = \case
  Enter -> False
  Update _ -> False
  View _ -> False
  Select ts -> all failing ts
  Change _ -> False
  Watch _ -> False

watching ::
  -- Collaborative r m => -- We need Collaborative here to pack references `r`
  Task m a ->
  List (Someref m) -- But there is no need to be in monad `m`
watching = \case
  Editor Unnamed _ -> []
  Editor (Named _) e -> watching' e
  Trans _ t2 -> watching t2
  Pair t1 t2 -> watching t1 `union` watching t2
  Done _ -> []
  Choose t1 t2 -> watching t1 `union` watching t2
  Fail -> []
  Step t1 _ -> watching t1
  Share _ -> []
  Assign _ _ -> []

watching' ::
  -- Collaborative r m => -- We need Collaborative here to pack references `r`
  Editor m a ->
  List (Someref m) -- But there is no need to be in monad `m`
watching' = \case
  Enter -> []
  Update _ -> []
  View _ -> []
  Select _ -> []
  Change (Store _ r) -> [pack r]
  Watch (Store _ r) -> [pack r]

-- | Calculate all possible options that can be send to this task,
-- | i.e. all possible label-activity pairs which select a task.
-- |
-- | Notes:
-- | * We need this to pair `Label`s with `Name`s, because we need to tag all deeper lables with the appropriate editor name.
-- | * We can't return a `HashMap _ (Task m a)` because deeper tasks can be of different types!
options ::
  Collaborative r m =>
  Task m a ->
  m (List (Input b)) -- We need to call `value`, therefore we are in `m`
options = \case
  Editor n (Select ts) -> pure <| map (IOption n) (options' ts)
  Editor _ _ -> pure []
  Trans _ t2 -> options t2
  Step t1 e2 -> pure (++) -< options t1 -< do
    mv1 <- value t1
    case mv1 of
      Nothing -> pure []
      Just v1 -> do
        let t2 = e2 v1 -- XXX Is this needed?
        options t2
  -- Pair t1 t2 -> pure [] --pure (++) -< options t1 -< options t2
  -- Choose t1 t2 -> pure [] --pure (++) -< options t1 -< options t2
  _ -> pure []

-- | Get all `Label`s out of a `Select` editor.
-- |
-- | Notes:
-- | * Goes one level deep!
options' ::
  HashMap Label (Task m a) ->
  List Label
options' = HashMap.keys << HashMap.filter (not << failing)

inputs ::
  Collaborative r m =>
  Task m a ->
  m (List (Input Dummy)) -- We need to call `value`, therefore we are in `m`
inputs t = case t of
  Editor Unnamed _ -> pure []
  Editor (Named n) (Select ts) -> options' ts |> map (ISelect n) |> pure
  Editor (Named n) e -> inputs' e |> map (IEnter n) |> pure
  Trans _ t2 -> inputs t2
  Pair t1 t2 -> pure (++) -< inputs t1 -< inputs t2
  Done _ -> pure []
  Choose t1 t2 -> pure (++) -< inputs t1 -< inputs t2
  Fail -> pure []
  Step t1 e2 -> pure (++) -< inputs t1 -< do
    mv1 <- value t1
    case mv1 of
      Nothing -> pure []
      Just v1 -> do
        let t2 = e2 v1
        options t2
  Share _ -> pure []
  Assign _ _ -> pure []

inputs' ::
  forall m a.
  -- Collaborative r m =>
  Editor m a ->
  List Dummy
inputs' t = case t of
  Enter -> [dummy beta]
  Update _ -> [dummy beta]
  View _ -> []
  Select _ -> [] --NOTE: selections do not have `IEnter` actions and are handles separately
  Change _ -> [dummy beta]
  Watch _ -> []
  where
    beta = Proxy :: Proxy a

-- Normalising -----------------------------------------------------------------

type Tracking f m a = WriterT (List (Someref m)) m (f m a)

normalise ::
  Collaborative r m =>
  MonadSupply Nat m =>
  MonadLog Steps m =>
  Task m a ->
  Tracking Task m a
normalise t = case t of
  -- Step --
  Step t1 e2 -> do
    t1' <- normalise t1
    let stay = Step t1' e2
    mv1 <- lift <| value t1'
    case mv1 of
      Nothing -> pure stay -- N-StepNone
      Just v1 -> do
        let t2 = e2 v1
        if failing t2
          then pure stay -- N-StepFail
          else do
            os <- lift <| options t2
            log Info <| DidCalculateOptions os (pretty t2 |> show)
            if not <| null <| os
              then pure stay -- N-StepWait
              else normalise t2 -- N-StepCont

  -- Choose --
  Choose t1 t2 -> do
    t1' <- normalise t1
    mv1 <- lift <| value t1'
    case mv1 of
      Just _ -> pure t1' -- N-ChooseLeft
      Nothing -> do
        t2' <- normalise t2
        mv2 <- lift <| value t2'
        case mv2 of
          Just _ -> pure t2' -- N-ChooseRight
          Nothing -> pure <| Choose t1' t2' -- N-ChooseNone

  -- Congruences --
  Trans f t1 -> pure (Trans f) -< normalise t1
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
    l <- lift <| share b
    pure <| Done l
  Assign b s@(Store _ r) -> do
    lift <| s <<- b
    tell [pack r]
    pure <| Done ()

-- balance :: Task m a -> Task m a
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

type PartialTracking f m a = WriterT (List (Someref m)) (ExceptT NotApplicable m) (f m a)

-- type PartialTrackingTask m a = ExceptT NotApplicable (WriterT (List (Someref m)) m) (Task m a)

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

handle ::
  forall m r a.
  Collaborative r m =>
  Task m a ->
  Input Concrete ->
  PartialTracking Task m a
handle t i = case t of
  -- Editors --
  Editor n e -> case i of
    IOption n' l -> case e of
      Select ts
        | n == n' -> case HashMap.lookup l ts of
          Nothing -> throw <| CouldNotFind l
          Just t' -> do
            os <- lift <| lift <| options t
            if i `elem` os
              then pure t'
              else throw <| CouldNotGoTo l
        | otherwise -> throw <| CouldNotMatch n n'
      _ -> throw <| CouldNotHandle i
    IEnter m b
      | n == Named m -> do
        e' <- handle' b e
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
        mv1 <- lift <| lift <| value t1
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
  forall m r a.
  Collaborative r m =>
  Concrete ->
  Editor m a -> -- `Select` does not return an `Editor`...
  PartialTracking Editor m a
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
      s <<- b'
      tell [pack r]
      pure <| Change s
    | otherwise -> throw <| CouldNotChangeRef (someTypeOf s) (someTypeOf b')
    where
      beta = typeRep :: TypeRep a
  -- Rest --
  _ -> throw <| CouldNotHandleValue c

-- Interaction -----------------------------------------------------------------

data Steps
  = DidStabilise Nat Nat
  | DidNotStabilise Nat Nat Nat
  | DidNormalise Text
  | DidBalance Text
  | DidStart Text
  | DidCalculateOptions (List (Input Dummy)) Text

instance Pretty Steps where
  pretty = \case
    DidNotStabilise d w o -> sep ["Found", pretty o, "overlap(s) amongst", pretty d, "dirty and", pretty w, "watched reference(s)"]
    DidStabilise d w -> sep ["Found no overlaps amongst", pretty d, "dirty and", pretty w, "watched reference(s)"]
    DidNormalise t -> sep ["Normalised to:", pretty t]
    DidBalance t -> sep ["Rebalanced to:", pretty t]
    DidStart t -> sep ["Started with:", pretty t]
    DidCalculateOptions os t -> sep ["Future options before normalising", pretty os, "for task", pretty t]

fixate ::
  Collaborative r m =>
  MonadSupply Nat m =>
  MonadLog Steps m =>
  Tracking Task m a ->
  m (Task m a)
fixate tt = do
  (t, d) <- runWriterT tt
  (t', d') <- runWriterT <| normalise t
  log Info <| DidNormalise (show <| pretty t')
  let ws = watching t'
  let ds = d `union` d'
  let os = ds `intersect` ws
  case os of
    -- F-Done --
    [] -> do
      log Info <| DidStabilise (length ds) (length ws)
      -- let t'' = balance t'
      -- log Info <| DidBalance (show <| pretty t'')
      pure t'
    -- F-Loop --
    _ -> do
      log Info <| DidNotStabilise (length ds) (length ws) (length os)
      fixate <| pure t'

initialise ::
  Collaborative r m =>
  MonadSupply Nat m =>
  MonadLog Steps m =>
  Task m a ->
  m (Task m a)
initialise t = do
  log Info <| DidStart (show <| pretty t)
  fixate (pure t)

interact ::
  Collaborative r m =>
  MonadSupply Nat m =>
  MonadLog NotApplicable m =>
  MonadLog Steps m =>
  Task m a ->
  Input Concrete ->
  m (Task m a)
interact t i = do
  xt <- runExceptT <| runWriterT <| handle t i
  case xt of
    Left e -> do
      log Warning e
      pure t
    Right tt -> fixate <| WriterT <| pure tt

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
