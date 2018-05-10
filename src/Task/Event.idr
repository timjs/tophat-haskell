module Task.Event

import Control.Catchable

import Task.Universe

%default total
%access public export


-- Paths -----------------------------------------------------------------------

namespace Path

    data Path
        = First
        | Second
        | Other Path

    Show Path where
        show First    = "f"
        show Second   = "s"
        show (Other p) = "s " ++ show p

    parse : List String -> Either String Path
    parse ["f"]         = pure First
    parse ["s"]         = pure Second
    parse ("s" :: rest) = map Other $ parse rest
    parse other           = throw $ "!! '" ++ unwords other ++ "' is not a valid path, type 'help' for more info"


-- Events ----------------------------------------------------------------------

data Action : Type where
    Change   : Universe.typeOf b -> Action
    Empty    : Action
    Pick     : Path -> Action
    Continue : Maybe Path -> Action

data Event
    = ToLeft Event
    | Here Action
    | ToRight Event


-- Showing ---------------------------------------------------------------------

Show Action where
    show (Change _)          = "change <val>"
    show Empty               = "empty"
    show (Pick p)            = "pick " ++ show p
    show (Continue Nothing)  = "cont"
    show (Continue (Just p)) = "cont " ++ show p

Show Event where
    show (ToLeft e)  = "l " ++ show e
    show (Here e)    = show e
    show (ToRight e) = "r " ++ show e


-- Parsing ---------------------------------------------------------------------

usage : String
usage = unlines
    [ ":: Possible events are:"
    , "    change <val> : change current editor to <val> "
    , "    empty        : empty current editor"
    , "    pick <path>  : choose amongst first or second option"
    , "    exec <path>  : execute one of possible options"
    , "    cont         : continue with the next task"
    , "    l <event>    : send <event> to the left task"
    , "    r <event>    : send <event> to the right task"
    , "    help         : show this message"
    ]

parse : List String -> Either String Event
parse ["change", val] with (Universe.Basic.parse val)
  parse ["change", val] | Nothing          = throw $ "!! Error parsing value '" ++ val ++ "'"
  parse ["change", val] | (Just (ty ** v)) = pure $ Here $ Change {b = BasicTy ty} v
parse ["empty"]                            = pure $ Here $ Empty
parse ("pick" :: rest)                     = map (Here . Pick) $ Path.parse rest
parse ["cont"]                             = pure . Here . Continue $ Nothing
parse ("cont" :: rest)                     = map (Here . Continue . Just) $ Path.parse rest
parse ("l" :: rest)                        = map ToLeft $ parse rest
parse ("r" :: rest)                        = map ToRight $ parse rest
parse ["help"]                             = throw usage
parse []                                   = throw ""
parse other                                = throw $ "!! '" ++ unwords other ++ "' is not a valid command, type 'help' for more info"
