module Task

import Task.Universe
import Task.Event

%default total
%access export

infix  6 =~, /~


-- Types -----------------------------------------------------------------------

-- State --

public export
StateTy : Universe.Ty
StateTy = ListTy (BasicTy IntTy)

public export
State : Type
State = typeOf StateTy


-- Tasks --

public export
data Task : Universe.Ty -> Type where
  -- Core
  Edit  : (val : Maybe (typeOf a)) -> Task a
  Watch : Task StateTy
  -- Parallel
  All   : Show (typeOf a) => Show (typeOf b) => (left : Task a) -> (right : Task b) -> Task (PairTy a b)
  -- Choice
  Any   : Show (typeOf a) => (left : Task a) -> (right : Task a) -> Task a
  One   : Show (typeOf a) => (left : Task a) -> (right : Task a) -> Task a
  Fail  : Task a
  -- Sequence
  Then  : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
  Next  : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
  -- Labels
  Label : Show (typeOf a) => Label -> (this : Task a) -> Task a
  -- State
  Get   : Task StateTy
  Put   : (x : typeOf StateTy) -> Task (BasicTy UnitTy)


-- Labels --

||| Get the current label, if one
label : Task a -> Maybe Label
label (Label l _) = Just l
label _           = Nothing

||| Remove as much labels as possible from a task.
|||
||| Usefull to deeply match task constructors while ignoring labels.
delabel : Task a -> Task a
delabel (Label _ t) = delabel t
delabel t           = t

||| Match a label to a task.
(=~) : Label -> Task a -> Bool
(=~) k (Label l _) = l == l
(=~) _ _           = False

||| Negation of `(=~)`.
(/~) : Label -> Task a -> Bool
(/~) l t = not (l =~ t)

||| Collect all labels in an external choice
labels : Task a -> List Label
labels (Label _ Fail)   = []
labels (Label l this)   = l :: labels this
labels (One left right) = labels left ++ labels right
-- --FIXME: should we also check for labels on the lhs of a step (see also `find`)?
-- labels (Then this _)    = labels this
-- labels (Next this _)    = labels this
labels _                = []

||| Depth first search for a label on a task tree.
|||
||| Returns the path of the found task.
find : Label -> Task a -> Maybe Path
find k (Label l this) with ( k == l )
  | True                = Just GoHere
  | False               = find k this
find k (One left right) = map GoLeft (find k left) <|> map GoRight (find k right)
-- --FIXME: should we can send pick-events through to the lhs of a step (see also `labels`)?
-- find k (Then this _)    = find k this
-- find k (Next this _)    = find k this
find k _                = Nothing

||| Check if a task constructor keeps its label after stepping or loses it.
keeper : Task a -> Bool
keeper (Edit _)  = True
keeper (All _ _) = True
keeper (Fail)    = True
keeper _         = False
