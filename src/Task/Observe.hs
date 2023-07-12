module Task.Observe where

import Data.List (union)
import qualified Data.Store as Store
import Polysemy
import Polysemy.Mutate (Alloc, Read)
import Task.Input
import Task.Syntax

---- Observations --------------------------------------------------------------
-- NOTE: Normalisation should never happen in any observation, they are immediate.

-- FIXME: should only be defined on NormalTask's
render ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` because `Value` needs it.
  NormalTask h a ->
  Sem r Text
render = \case
  NormalEdit k e -> render' k e
  NormalSelect k t1 cs -> done (\l -> concat [l, " ▷^", display k, " ", display <| keys cs]) -<< render t1
  NormalPool k _ ts -> done (\is -> unwords ["⋈^", display k, display is]) -<< traverse render ts
  NormalLift _ -> done "■ …"
  NormalPair t1 t2 -> done (\l r -> unwords [l, " ⧓ ", r]) -<< render t1 -<< render t2
  NormalChoose t1 t2 -> done (\l r -> unwords [l, " ◆ ", r]) -<< render t1 -<< render t2
  NormalFail -> done "↯"
  NormalTrans _ t -> render t
  NormalStep t1 _ -> done (\l -> concat [l, " ▶…"]) -<< render t1

render' ::
  (Members '[Read h] r) => -- We need `Read` to read from references.
  Id ->
  Editor h b ->
  Sem r Text
render' n = \case
  Enter -> done <| concat ["□^", display n, " [ ] "]
  Update b -> done <| concat ["⊟^", display n, " [ ", display b, " ]"]
  View b -> done <| concat ["⧇^", display n, " [ ", display b, " ]"]
  Change l -> done (\b -> concat ["^⊞", display n, " [ ", display b, " ]"]) -<< Store.read l
  Watch l -> done (\b -> concat ["⧈^", display n, " [ ", display b, " ]"]) -<< Store.read l

value ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` to allocate a fresh store to continue with.
  NormalTask h a ->
  Sem r (Maybe a)
value = \case
  NormalEdit _ e -> value' e
  NormalSelect _ _ _ -> done Nothing
  NormalPool _ _ ts -> sequence <-< traverse value ts
  NormalTrans f t -> done (map f) -<< value t
  NormalPair t1 t2 -> done (<&>) -<< value t1 -<< value t2
  NormalLift v -> done (Just v)
  NormalChoose t1 t2 -> done (<|>) -<< value t1 -<< value t2
  NormalFail -> done Nothing
  NormalStep _ _ -> done Nothing

value' ::
  (Members '[Read h] r) => -- We need `Read` to read from references.
  Editor h a ->
  Sem r (Maybe a)
value' = \case
  Enter -> done Nothing
  Update b -> done (Just b)
  View b -> done (Just b)
  Change l -> done Just -<< Store.read l
  Watch l -> done Just -<< Store.read l

failing ::
  Task h a ->
  Bool
failing = \case
  Edit _ _ -> False
  Select _ t1 _ -> failing t1
  Pool _ _ _ -> False -- Always able to start a new task
  Trans _ t2 -> failing t2
  Pair t1 t2 -> failing t1 && failing t2
  Lift _ -> False
  Choose t1 t2 -> failing t1 && failing t2
  Fail -> True
  Step t1 _ -> failing t1
  Assert _ -> False
  Share _ -> False
  Assign _ _ -> False

watching ::
  NormalTask h a ->
  List (Some (Ref h)) -- There is no need for any effects.
watching = \case
  NormalEdit _ e -> watching' e
  NormalSelect _ t1 _ -> watching t1
  NormalPool _ _ ts -> ts |> map watching |> foldr union []
  NormalTrans _ t2 -> watching t2
  NormalPair t1 t2 -> watching t1 `union` watching t2
  NormalLift _ -> []
  NormalChoose t1 t2 -> watching t1 `union` watching t2
  NormalFail -> []
  NormalStep t1 _ -> watching t1

watching' ::
  Editor h a ->
  List (Some (Ref h)) -- There is no need for any effects.
watching' = \case
  Enter -> []
  Update _ -> []
  View _ -> []
  Change (Store _ r) -> [pack r]
  Watch (Store _ r) -> [pack r]

inputs ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` and `Read` because we call `value`.
  NormalTask h a ->
  Sem r (List (Input Abstract))
inputs t = case t of
  NormalEdit k e -> done <| map (Send k) (inputs' e)
  NormalSelect k t1 ts ->
    done (++) -<< inputs t1 -<< do
      mv1 <- value t1
      case mv1 of
        Nothing -> done []
        Just v1 -> done [Send k (Decide l) | (l, e) <- ts, not <| failing (e v1)]
  NormalPool k _ ts -> do
    let l = length ts
    let is = map (Send k) ([Init] ++ [Fork i | i <- [1 .. l]] ++ [Kill i | i <- [1 .. l]])
    is' <- concat <-< traverse inputs ts
    done <| is ++ is'
  NormalTrans _ t2 -> inputs t2
  NormalPair t1 t2 -> done (++) -<< inputs t1 -<< inputs t2
  NormalLift _ -> done []
  NormalChoose t1 t2 -> done (++) -<< inputs t1 -<< inputs t2
  NormalFail -> done []
  NormalStep t1 _ -> inputs t1

inputs' ::
  forall h a.
  Editor h a ->
  List (Action Abstract)
inputs' t = case t of
  Enter -> [Insert <| Abstract beta]
  Update _ -> [Insert <| Abstract beta]
  View _ -> []
  Change _ -> [Insert <| Abstract beta]
  Watch _ -> []
  where
    beta = Proxy :: Proxy a
