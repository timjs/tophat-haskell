module Data.Task.Observe where

import qualified Data.HashMap.Strict as HashMap
import Data.List (union)
import Data.Task
import Data.Task.Input

---- Observations --------------------------------------------------------------
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
      Just v1 -> pure <| options (e2 v1)
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
  Enter -> pure <| cat ["⊠^", pretty a, " [ ] "]
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
  Done v -> pure (Just v)
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
  Trans _ t2 -> failing t2
  Pair t1 t2 -> failing t1 && failing t2
  Done _ -> False
  Choose t1 t2 -> failing t1 && failing t2
  Fail -> True
  Step t1 _ -> failing t1
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
  List Option
options = \case
  Editor n (Select ts) -> options' ts |> map (Option n)
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
        options t2 |> map fromOption |> pure
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
