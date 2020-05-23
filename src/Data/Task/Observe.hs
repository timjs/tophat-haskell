module Data.Task.Observe where

import qualified Data.HashMap.Strict as HashMap
import Data.List (union)
import Data.Task
import Data.Task.Input
import Polysemy
import Polysemy.Mutate

-- Observations ----------------------------------------------------------------
-- NOTE: Normalisation should never happen in any observation, they are immediate.

ui ::
  Members '[Alloc h, Read h] r =>
  Act h (Sem r) a ->
  Sem r (Doc n)
ui = \case
  Edit a e -> ui' a e
  Done _ -> pure "■ …"
  Pair t1 t2 -> pure (\l r -> sep [l, " ⧓ ", r]) -< ui t1 -< ui t2
  Choose t1 t2 -> pure (\l r -> sep [l, " ◆ ", r]) -< ui t1 -< ui t2
  Fail -> pure "↯"
  Trans _ t -> ui t
  Step t1 e2 -> pure go -< ui t1 -< do
    mv1 <- value t1
    case mv1 of
      Nothing -> pure []
      Just v1 -> pure <| options (e2 v1)
    where
      go s ls
        | null ls = cat [s, " ▶…"]
        | otherwise = cat [s, " ▷", pretty ls]
  Share b -> pure <| sep ["share", pretty b]
  Assign b _ -> pure <| sep ["…", ":=", pretty b]

ui' ::
  Member (Read h) r =>
  Name ->
  Edit h (Sem r) b ->
  Sem r (Doc n)
ui' a = \case
  Enter -> pure <| cat ["⊠^", pretty a, " [ ] "]
  Update b -> pure <| cat ["□^", pretty a, " [ ", pretty b, " ]"]
  View b -> pure <| cat ["⧇^", pretty a, " [ ", pretty b, " ]"]
  Select ts -> pure <| cat ["◇^", pretty a, " ", pretty <| HashMap.keysSet ts]
  Change l -> pure (\b -> cat ["⊟^", pretty a, " [ ", pretty b, " ]"]) -< deref l
  Watch l -> pure (\b -> cat ["⧈^", pretty a, " [ ", pretty b, " ]"]) -< deref l

value ::
  Members '[Alloc h, Read h] r =>
  Act h (Sem r) a ->
  Sem r (Maybe a) -- We need to watch locations and be in monad `m`
value = \case
  Edit Unnamed _ -> pure Nothing
  Edit (Named _) e -> value' e
  Trans f t -> pure (map f) -< value t
  Pair t1 t2 -> pure (><) -< value t1 -< value t2
  Done v -> pure (Just v)
  Choose t1 t2 -> pure (<|>) -< value t1 -< value t2
  Fail -> pure Nothing
  Step _ _ -> pure Nothing
  Share b -> pure Just -< ref b -- Nothing??
  Assign _ _ -> pure (Just ()) -- Nothing??

value' ::
  Member (Read h) r => -- We need to deref locations and be in monad `m`
  Edit h (Sem r) a ->
  Sem r (Maybe a)
value' = \case
  Enter -> pure Nothing
  Update b -> pure (Just b)
  View b -> pure (Just b)
  Select _ -> pure Nothing
  Change l -> pure Just -< deref l
  Watch l -> pure Just -< deref l

failing ::
  Act h m a ->
  Bool
failing = \case
  Edit _ e -> failing' e
  Trans _ t2 -> failing t2
  Pair t1 t2 -> failing t1 && failing t2
  Done _ -> False
  Choose t1 t2 -> failing t1 && failing t2
  Fail -> True
  Step t1 _ -> failing t1
  Share _ -> False
  Assign _ _ -> False

failing' ::
  Edit h m a ->
  Bool
failing' = \case
  Enter -> False
  Update _ -> False
  View _ -> False
  Select ts -> all failing ts
  Change _ -> False
  Watch _ -> False

watching ::
  Act h m a ->
  List (Someref m) -- There is no need to be in monad `m`
watching = \case
  Edit Unnamed _ -> []
  Edit (Named _) e -> watching' e
  Trans _ t2 -> watching t2
  Pair t1 t2 -> watching t1 `union` watching t2
  Done _ -> []
  Choose t1 t2 -> watching t1 `union` watching t2
  Fail -> []
  Step t1 _ -> watching t1
  Share _ -> []
  Assign _ _ -> []

watching' ::
  Edit h m a ->
  List (Someref m) -- There is no need to be in monad `m`
watching' = \case
  Enter -> []
  Update _ -> []
  View _ -> []
  Select _ -> []
  Change r -> [pack r]
  Watch r -> [pack r]

-- | Calculate all possible options that can be send to this task,
-- | i.e. all possible label-activity pairs which select a task.
-- |
-- | Notes:
-- | * We need this to pair `Label`s with `Name`s, because we need to tag all deeper lables with the appropriate editor name.
-- | * We can't return a `HashMap _ (Act h m a)` because deeper tasks can be of different types!
options ::
  Act h m a ->
  List Option
options = \case
  Edit n (Select ts) -> options' ts |> map (Option n)
  Trans _ t2 -> options t2
  Step t1 _ -> options t1
  -- Step t1 e2 -> pure (++) -< options t1 -< do
  --   mv1 <- value t1
  --   case mv1 of
  --     Nothing -> pure []
  --     Just v1 -> do
  --       let t2 = e2 v1
  --       options t2
  -- Pair t1 t2 -> pure [] --pure (++) -< options t1 -< options t2
  -- Choose t1 t2 -> pure [] --pure (++) -< options t1 -< options t2
  _ -> []

-- | Get all `Label`s out of a `Select` editor.
-- |
-- | Notes:
-- | * Goes one level deep!
options' ::
  HashMap Label (Act h m a) ->
  List Label
options' = HashMap.keys << HashMap.filter (not << failing)

inputs ::
  Members '[Alloc h, Read h] r =>
  Act h (Sem r) a ->
  Sem r (List (Input Dummy)) -- We need to call `value`, therefore we are in `m`
inputs t = case t of
  Edit Unnamed _ -> pure []
  Edit (Named n) (Select ts) -> options' ts |> map (ISelect n) |> pure
  Edit (Named n) e -> inputs' e |> map (IEnter n) |> pure
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
        options t2 |> map fromOption |> pure
  Share _ -> pure []
  Assign _ _ -> pure []

inputs' ::
  forall h m a.
  Edit h m a ->
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
