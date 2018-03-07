module Task.Event

import Task.Type

%default total
%access public export


-- Events ----------------------------------------------------------------------

data Action : Type where
    Change   : {b : Ty} -> typeOf b -> Action
    Clear    : Action
    Continue : Action

data Event
    = ToLeft Event
    | ToRight Event
    | Here Action


-- Parsing ---------------------------------------------------------------------

usage : String
usage = unlines
    [ ":: Possible events are:"
    , "    change <val> : change current value to <val> "
    , "    clear        : clear current value"
    , "    cont         : continue with the next task"
    , "    lft <event>  : send <event> to the left task"
    , "    rgt <event>  : send <event> to the right task"
    , "    help         : show this message"
    ]

parse : List String -> Either String Event
parse ["change", val] with (Type.parse val)
  parse ["change", val] | Nothing          = Left $ "!! Error parsing value '" ++ val ++ "'"
  parse ["change", val] | (Just (ty ** v)) = Right $ Here $ Change {b = Basic ty} v
parse ["clear"]                            = Right $ Here $ Clear
parse ["cont"]                             = Right $ Here $ Continue
parse ("lft" :: rest)                      = map ToLeft $ parse rest
parse ("rgt" :: rest)                      = map ToRight $ parse rest
parse ["help"]                             = Left usage
parse []                                   = Left ""
parse other                                = Left $ "!! '" ++ unwords other ++ "' is not a valid command, type 'help' for more info"
