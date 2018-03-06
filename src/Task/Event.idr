module Task.Event

import Task.Type

%default total
%access public export


-- Events ----------------------------------------------------------------------

data Event : Type where
    Change   : {b : Ty} -> typeOf b -> Event
    Clear    : Event
    Continue : Event
    First    : Event -> Event
    Second   : Event -> Event


-- Parsing ---------------------------------------------------------------------

usage : String
usage = unlines
    [ ":: Possible events are:"
    , "    change <val> : change current value to <val> "
    , "    clear        : clear current value"
    , "    cont         : continue with the next task"
    , "    fst <event>  : send <event> to the first task"
    , "    snd <event>  : send <event> to the second task"
    , "    help         : show this message"
    ]

parse : List String -> Either String Event
parse ["change", val] with (Type.parse val)
  parse ["change", val] | Nothing          = Left $ "!! Error parsing value '" ++ val ++ "'"
  parse ["change", val] | (Just (ty ** v)) = Right $ Change {b = Basic ty} v
parse ["clear"]                            = Right $ Clear
parse ["cont"]                             = Right $ Continue
parse ("fst" :: rest)                      = map First $ parse rest
parse ("snd" :: rest)                      = map Second $ parse rest
parse ["help"]                             = Left usage
parse []                                   = Left ""
parse other                                = Left $ "!! '" ++ unwords other ++ "' is not a valid command, type 'help' for more info"
