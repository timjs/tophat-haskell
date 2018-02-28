module Task


-- Types -----------------------------------------------------------------------

ID : Type
ID = Int

UI : Type
UI = String

State : Type
State = Int


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
  Par : ID -> Task a -> Task b -> Task ( a, b )
  -- User interaction
  Enter : ID -> Value a -> Task a
  Show : ID -> Task a
  -- Share interaction
  Get : ID -> Task State
  Put : ID -> State -> Task ()

pure : a -> Task a
pure x =
  Pure x

unit : Task ()
unit =
  pure ()


-- Semantics -------------------------------------------------------------------

ui : Task a -> UI
ui t =
  ?ui

normalise : State -> Task a -> ( State, Task a )
normalise state task =
  case task of
    -- Combinators
    Seq id left func =>
      let
        ( newState, newLeft ) = Task.normalise state left
      in
      case newLeft of
        Pure a =>
          ( newState, func a )
        _ =>
          ( newState, Seq id newLeft func )
    Par id left right =>
      let
        ( newState, newLeft ) = Task.normalise state left
        ( newerState, newRight ) = Task.normalise newState right
      in
      case ( newLeft, newRight ) of
        ( Pure a, Pure b ) =>
          ( newerState, Pure ( a, b ) )
        ( newLeft, newRight ) =>
          ( newerState, Par id newLeft newRight )
    -- State
    Get id =>
      ( state, pure state )

    Put id val =>
      ( val, unit )
    -- Pure, Enter and Show are values
    _ =>
      ( state, task )

value : State -> Task a -> ( State, Value a )
value state task =
  ?value

step : State -> Event b -> Task a -> ( State, Task a )
step state event task =
  ?step
