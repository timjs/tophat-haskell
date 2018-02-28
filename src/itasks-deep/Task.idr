module Task

%default total


-- Universe --------------------------------------------------------------------

data TaskType
     = UNIT
     | INT
     | PAIR TaskType TaskType

valueOf : TaskType -> Type
valueOf UNIT       = ()
valueOf INT        = Int
valueOf (PAIR a b) = ( valueOf a, valueOf b )

snd_neq : (contra : (y = y') -> Void) -> (PAIR x y = PAIR x y') -> Void
snd_neq contra Refl = contra Refl

fst_neq : (contra : (x = x') -> Void) -> (PAIR x y = PAIR x' y) -> Void
fst_neq contra Refl = contra Refl

both_neq : (contra_x : (x = x') -> Void) -> (contra_y : (y = y') -> Void) -> (PAIR x y = PAIR x' y') -> Void
both_neq contra_x contra_y Refl = contra_x Refl

Uninhabited (UNIT = INT) where
            uninhabited Refl impossible

Uninhabited (UNIT = PAIR x y) where
            uninhabited Refl impossible

Uninhabited (INT = PAIR x y) where
            uninhabited Refl impossible

DecEq TaskType where
      decEq UNIT       UNIT                                             = Yes Refl
      decEq INT        INT                                              = Yes Refl
      decEq (PAIR x y) (PAIR x' y')     with (decEq x x')
        decEq (PAIR x y) (PAIR x y')    | (Yes Refl)  with (decEq y y')
          decEq (PAIR x y) (PAIR x y)   | (Yes Refl)  | (Yes Refl)      = Yes Refl
          decEq (PAIR x y) (PAIR x y')  | (Yes Refl)  | (No contra)     = No (snd_neq contra)
        decEq (PAIR x y) (PAIR x' y')   | (No contra) with (decEq y y')
          decEq (PAIR x y) (PAIR x' y)  | (No contra) | (Yes Refl)      = No (fst_neq contra)
          decEq (PAIR x y) (PAIR x' y') | (No contra) | (No contra')    = No (both_neq contra contra')
      decEq UNIT       INT                                              = No absurd
      decEq INT        UNIT                                             = No (negEqSym absurd)
      decEq UNIT       (PAIR x y)                                       = No absurd
      decEq (PAIR x y) UNIT                                             = No (negEqSym absurd)
      decEq INT        (PAIR x y)                                       = No absurd
      decEq (PAIR x y) INT                                              = No (negEqSym absurd)


-- Types -----------------------------------------------------------------------

Id : Type
Id = Int

State : Type
State = Int


-- Values --

data Value : TaskType -> Type where
     NoValue   : Value a
     JustValue : {a : TaskType} -> valueOf a -> Value a

coerce : (a = b) -> Value a -> Value b
coerce Refl x = x


-- Events --

data Event : Type where
     Change   : Id -> (a ** Value a) -> Event
     Continue : Id -> Event


-- Tasks --

data Task : TaskType -> Type where
     -- Primitive combinators
     Seq  : Id -> Task a -> (valueOf a -> Task b) -> Task b
     Par  : Task a -> Task b -> Task (PAIR a b)
     -- User interaction
     Edit : Id -> Value a -> Task a
     -- Share interaction
     Get  : Task INT
     Put  : valueOf INT -> Task UNIT
     -- Lifting
     Pure : {a : TaskType} -> valueOf a -> Task a

pure : {a : TaskType} -> valueOf a -> Task a
pure = Pure

unit : Task UNIT
unit = pure ()


-- Showing ---------------------------------------------------------------------

Show (valueOf a) => Show (Value a) where
     show NoValue       = "<no value>"
     show (JustValue x) = show x

Show (valueOf a) => Show (Task a) where
     show (Seq id left cont) = ?show_seq ++ " =>_" ++ show id ++ " <cont>"
     show (Par left right)   = ?show_par ++ "<par>"
     show (Edit id val)      = "<edit_" ++ show id ++ " " ++ show val ++ ">"
     show (Get)              = "<get>"
     show (Put x)            = "<put " ++ show x ++ ">"
     show (Pure x)           = show x

-- Semantics -------------------------------------------------------------------

value : Task a -> Value a
value (Pure x)     = JustValue x
value (Edit _ val) = val
value _            = NoValue

normalise : Task a -> State -> ( Task a, State )
-- Combinators
normalise (Seq id left cont) state =
          let
               ( newLeft, newState ) = normalise left state
          in
          case newLeft of
               --FIXME: maybe add a normalise here
               Pure a => ( cont a, newState )
               _      => ( Seq id newLeft cont, newState )
normalise (Par left right) state =
          let
               ( newLeft, newState )    = normalise left state
               ( newRight, newerState ) = normalise right newState
          in
          case ( newLeft, newRight ) of
               ( Pure a, Pure b )    => ( Pure ( a, b ), newerState )
               ( newLeft, newRight ) => ( Par newLeft newRight, newerState )
-- State
normalise (Get) state =
          ( pure state, state )
normalise (Put x) state =
          ( unit, x )
-- Values
normalise task state =
          ( task, state )

handle : Task a -> Event -> State -> ( Task a, State )
handle task@(Seq id left cont) event@(Continue eventId) state =
       -- If we pressed Continue...
       if id == eventId then
            -- ...and the id's match...
            case value left of
                 -- ...and we have a value: we get on with the continuation
                 JustValue v => ( cont v, state )
                 -- ...without a value: we stay put.
                 NoValue     => ( task, state )
       else
            -- ...but the id's dont' match: we bubble the event down.
            -- This covers the case that `left` consists of parallels containing `Seq`!
            let
                 ( newLeft, newState ) = handle left event state
            in
            ( Seq id newLeft cont, newState )
handle task@(Par left right) event state =
       -- We pass on the event to left and right in sequence
       let
            ( newLeft, newState )    = handle left event state
            ( newRight, newerState ) = handle right event newState
       in
       ( Par newLeft newRight, newerState )
handle task@(Edit {a} id val) (Change eventId (b ** newVal)) state =
       case decEq b a of
            Yes prf =>
                 if id == eventId then
                      ( Edit id (coerce prf newVal), state )
                 else
                      ( task, state )
            No _ =>
                 ( task, state )
handle task@(Pure _) _ state =
       -- In this case evaluation terminated
       ( task, state )
handle task@(Get) _ state =
       -- This case can't happen, it is already evaluated by `normalise`
       --FIXME: express this in the type system
       ( task, state )
handle task@(Put _) _ state =
       -- This case can't happen
       --FIXME: express this in the type system
       ( task, state )
