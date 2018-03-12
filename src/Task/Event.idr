module Task.Event

import Task.Type

%default total
%access public export


-- Events ----------------------------------------------------------------------

data Path
    = PF
    | PS
    | PN Path

data Action : Type where
    Change   : typeOf b -> Action
    Clear    : Action
    Pick     : Path -> Action
    Execute  : Path -> Action
    Continue : Action

data Event
    = ToLeft Event
    | Here Action
    | ToRight Event


-- Parsing ---------------------------------------------------------------------

usage : String
usage = unlines
    [ ":: Possible events are:"
    , "    change <val>  : change current value to <val> "
    , "    clear         : clear current value"
    , "    pick <path>   : choose amongst first or second option"
    , "    exec <path>   : execute one of possible options"
    , "    cont          : continue with the next task"
    , "    left <event>  : send <event> to the left task"
    , "    right <event> : send <event> to the right task"
    , "    help          : show this message"
    ]

parsePath : List String -> Either String Path
parsePath ["f"]         = Right PF
parsePath ["s"]         = Right PS
parsePath ("s" :: rest) = map PN $ parsePath rest
parsePath other         = Left $ "!! '" ++ unwords other ++ "' is not a valid path, type 'help' for more info"

parse : List String -> Either String Event
parse ["change", val] with (Type.parse val)
  parse ["change", val] | Nothing          = Left $ "!! Error parsing value '" ++ val ++ "'"
  parse ["change", val] | (Just (ty ** v)) = Right $ Here $ Change {b = Basic ty} v
parse ["clear"]                            = Right $ Here $ Clear
parse ("pick" :: rest)                     = map (Here . Pick) $ parsePath rest
parse ("exec" :: rest)                     = map (Here . Execute) $ parsePath rest
parse ["cont"]                             = Right $ Here $ Continue
parse ("left" :: rest)                     = map ToLeft $ parse rest
parse ("right" :: rest)                    = map ToRight $ parse rest
parse ["help"]                             = Left usage
parse []                                   = Left ""
parse other                                = Left $ "!! '" ++ unwords other ++ "' is not a valid command, type 'help' for more info"
