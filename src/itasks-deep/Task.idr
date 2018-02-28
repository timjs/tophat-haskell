module Task


-- Types -----------------------------------------------------------------------

ID : Type
ID = Int

UI : Type
UI = String


-- Values --

data Value : Type -> Type where
    NoValue : Value a
    JustValue : a -> Value a


-- Events --

data Event : Type -> Type where
    Change : ID -> Value a -> Event a
    Commit : ID -> Event a
    Continue : ID -> Event a


-- Tasks --

data Task : Type -> Type where
    -- Primitive combinators
    Pure : a -> Task a
    Seq : ID -> Task a -> (a -> Task b) -> Task b
    Par : ID -> Task a -> Task b -> Task (a, b)
    -- User interaction
    Edit : ID -> Value a -> Task a
    View : ID -> Value a -> Task ()
    -- Share interaction
    Get : ID -> Task a
    Put : ID -> a -> Task ()


-- Semantics -------------------------------------------------------------------

ui : Task a -> UI
ui t =
    ?ui

step : Event b -> Task a -> Task a
step event task =
    ?step

value : Task a -> Value a
value task =
    ?value

normalise : Task a -> Task a
normalise task =
    ?normalise
