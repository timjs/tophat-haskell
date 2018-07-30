module Task.Event

import Control.Catchable

import Task.Universe

%default total
%access public export


-- Labels ----------------------------------------------------------------------

Label : Type
Label = String


-- Events ----------------------------------------------------------------------

data Action : Type where
  Change   : Universe.typeOf b -> Action
  Clear    : Action
  Pick     : Label -> Action
  Continue : Maybe Label -> Action

data Event
  = ToLeft Event
  | ToHere Action
  | ToRight Event


-- Showing ---------------------------------------------------------------------

Show Action where
  show (Change _)          = "change <val>"
  show (Clear)             = "clear"
  show (Pick l)            = "pick " ++ show l
  show (Continue Nothing)  = "cont"
  show (Continue (Just l)) = "cont " ++ show l

Show Event where
  show (ToLeft e)  = "f " ++ show e
  show (ToHere a)   = show a
  show (ToRight e) = "s " ++ show e


-- Parsing ---------------------------------------------------------------------

usage : String
usage = unlines
  [ ":: Possible events are:"
  , "    change <val> : change current editor to <val> "
  , "    clear        : clear current editor"
  , "    pick <label> : choose amongst the possible options"
  , "    cont         : continue with the next task"
  , "    cont <label> : continue with one of possible options"
  , "    l <event>    : send <event> to the left task"
  , "    r <event>    : send <event> to the right task"
  , "    help         : show this message"
  , ""
  , "where values <val> can be:"
  , "    True, False (Booleans)"
  , "    1, 32, -42  (Integers)"
  , "    \"Hello\"   (Strings)"
  ]

parse : List String -> Either String Event
parse ["change", val] with (Universe.Basic.parse val)
  parse ["change", val] | Nothing          = throw $ "!! Error parsing value '" ++ val ++ "'"
  parse ["change", val] | (Just (ty ** v)) = pure $ ToHere $ Change {b = BasicTy ty} v
parse ["clear"]                            = pure $ ToHere $ Clear
parse ["pick", label]                      = pure $ ToHere $ Pick label
parse ["cont"]                             = pure $ ToHere $ Continue Nothing
parse ["cont", label]                      = pure $ ToHere $ Continue (Just label)
parse ("l" :: rest)                        = map ToLeft $ parse rest
parse ("r" :: rest)                        = map ToRight $ parse rest
parse ["help"]                             = throw usage
parse []                                   = throw ""
parse other                                = throw $ "!! '" ++ unwords other ++ "' is not a valid command, type 'help' for more info"
