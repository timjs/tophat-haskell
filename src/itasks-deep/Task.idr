module Task


-- Types -----------------------------------------------------------------------

Id : Type
Id = Int

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
  Change : Id -> Value a -> Event a
  Continue : Id -> Event a


-- Tasks --

data Task : Type -> Type where
  -- Lifting
  Pure : a -> Task a
  -- Primitive combinators
  Seq : Id -> Task a -> (a -> Task b) -> Task b
  Par : Task a -> Task b -> Task ( a, b )
  -- User interaction
  Edit : Id -> Value a -> Task a
  -- Share interaction
  Get : Task State
  Put : State -> Task ()

pure : a -> Task a
pure x =
  Pure x

unit : Task ()
unit =
  pure ()


-- Showing ---------------------------------------------------------------------

Show a => Show (Value a) where
  show value =
    case value of
      NoValue =>
        "<no value>"
      JustValue x =>
        show x

Show a => Show (Task a) where
  show task =
    case task of
      Seq id left _ =>
        -- show left ++
        " =>_" ++ show id ++ " <cont>"
      Par left right =>
        -- show left ++ " || " ++ show' right where
        "<par>"
      Edit id val =>
        "<edit_" ++ show id ++ " " ++ show val ++ ">"
      Pure x =>
        show x
      Get =>
        "<get>"
      Put x =>
        "<put " ++ show x ++ ">"

-- Semantics -------------------------------------------------------------------

value : Task a -> Value a
value task =
  case task of
    Pure x =>
      JustValue x
    Edit _ val =>
      val
    _ =>
      NoValue

normalise : Task a -> State -> ( Task a, State )
normalise task state =
  case task of
    -- Combinators
    Seq id left cont =>
      let
        ( newLeft, newState ) = normalise left state
      in
      case newLeft of
        Pure a =>
          --FIXME: add a normalise here???
          ( cont a, newState )
        _ =>
          ( Seq id newLeft cont, newState )
    Par left right =>
      let
        ( newLeft, newState ) = normalise left state
        ( newRight, newerState ) = normalise right newState
      in
      case ( newLeft, newRight ) of
        ( Pure a, Pure b ) =>
          ( Pure ( a, b ), newerState )
        ( newLeft, newRight ) =>
          ( Par newLeft newRight, newerState )
    -- State
    Get =>
      ( pure state, state )

    Put newState =>
      ( unit, newState )
    -- Pure and Edit are values
    _ =>
      ( task, state )

handle : Task a -> State -> Event b -> ( Task a, State )
handle task state event =
  case ( task, event ) of
    ( Seq id left cont, Continue eventId ) =>
      -- If we pressed Continue...
      if id == eventId then
        -- ...and the id's match...
        case value left of
          -- ...and we have a value: we get on with the continuation
          JustValue v =>
            ( cont v, state )
          -- ...withaout a value: we stay put.
          NoValue =>
            ( task, state )
      else
        -- ...but the id's dont' match: we bubble the event down.
        -- This covers the case that `left` consists of parallels containing `Seq`!
        let
          ( newTask, newState ) = handle task state event
        in
        ( newTask, newState )
    ( Par left right, e ) =>
      -- We pass on the event to left and right in sequence
      let
        ( newLeft, newState ) = handle left state event
        ( newRight, newerState ) = handle right newState event
      in
      ( Par newLeft newRight, newerState )
    ( Edit id val, Change eventId newVal ) =>
      if id == eventId then
        ( ?newEdit , state ) --Edit id newVal, state )
      else
        ( task, state )
    ( Pure _, _ ) =>
      -- In this case evaluation terminated
      ( task, state )
    ( Get, _ ) =>
      -- This case can't happen, it is already evaluated by `normalise`
      --FIXME: express this in the type system
      ( task, state )
    ( Put _, _ ) =>
      -- This case can't happen
      ( task, state )
