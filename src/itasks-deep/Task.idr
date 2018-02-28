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
  -- Lifting
  Pure : a -> Task a
  -- Primitive combinators
  Seq : ID -> Task a -> (a -> Task b) -> Task b
  Par : ID -> Task a -> Task b -> Task ( a, b )
  -- User interaction
  Edit : ID -> Value a -> Task a
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

normalise : Task a -> State -> ( Task a, State )
normalise task state =
  case task of
    -- Combinators
    Seq id left cont =>
      let
        ( newLeft, newState ) = Task.normalise left state
      in
      case newLeft of
        Pure a =>
          ( cont a, newState )
        _ =>
          ( Seq id newLeft cont, newState )
    Par id left right =>
      let
        ( newLeft, newState ) = Task.normalise left state
        ( newRight, newerState ) = Task.normalise right newState
      in
      case ( newLeft, newRight ) of
        ( Pure a, Pure b ) =>
          ( Pure ( a, b ), newerState )
        ( newLeft, newRight ) =>
          ( Par id newLeft newRight, newerState )
    -- State
    Get id =>
      ( pure state, state )

    Put id newState =>
      ( unit, newState )
    -- Pure and Edit are values
    _ =>
      ( task, state )

ui : Task a -> State -> UI
ui task state =
  let
    -- Drawing the UI needs normalisation first
    ( newTask, newState ) = normalise task state
  in
  ?ui

value : Task a -> Value a
value task =
  case task of
    Pure x =>
      JustValue x
    Edit id val =>
      val
    _ =>
      NoValue

step : Task a -> State -> Event b -> ( Task a, State )
step task state event =
  case ( task, event ) of
    ( Seq id left cont, Continue eventId ) =>
      ?h1
      -- if id == eventId then
      --   -- If we pressed Continue, and the id's match, we get on with the continuation.
      --   ( cont (value left), state )
      -- else
      --   -- Otherwise we pass stay put
      --   ( newTask, state )
    ( Par id left right, e ) =>
      ?h2
    ( Pure x, e ) =>
      ?h3
    ( Edit x y, e ) =>
      ?h4
    ( Get x, e ) =>
      ?h5
    ( Put x y, e ) =>
      ?h6
