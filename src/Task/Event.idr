module Task.Event

import Control.Catchable
import Task.Type

%default total
%access public export


-- Paths -----------------------------------------------------------------------

namespace Path

    data Path
        = First
        | Second
        | Next Path

    parse : List String -> Either String Path
    parse ["fst"]         = pure First
    parse ["snd"]         = pure Second
    parse ("snd" :: rest) = map Next $ parse rest
    parse other           = throw $ "!! '" ++ unwords other ++ "' is not a valid path, type 'help' for more info"


-- Events ----------------------------------------------------------------------

data Action : Type where
    Change   : Type.typeOf b -> Action
    Clear    : Action
    Choose   : Path -> Action
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

parse : List String -> Either String Event
parse ["change", val] with (Type.Basic.parse val)
  parse ["change", val] | Nothing          = throw $ "!! Error parsing value '" ++ val ++ "'"
  parse ["change", val] | (Just (ty ** v)) = pure $ Here $ Change {b = BasicTy ty} v
parse ["clear"]                            = pure $ Here $ Clear
parse ("choose" :: rest)                   = map (Here . Choose) $ Path.parse rest
parse ("exec" :: rest)                     = map (Here . Execute) $ Path.parse rest
parse ["cont"]                             = pure $ Here $ Continue
parse ("left" :: rest)                     = map ToLeft $ parse rest
parse ("right" :: rest)                    = map ToRight $ parse rest
parse ["help"]                             = throw usage
parse []                                   = throw ""
parse other                                = throw $ "!! '" ++ unwords other ++ "' is not a valid command, type 'help' for more info"
