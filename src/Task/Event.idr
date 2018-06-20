module Task.Event

import Control.Catchable

import Task.Universe

%default total
%access public export


-- Paths -----------------------------------------------------------------------

namespace Path

    data Path
        = GoLeft Path
        | GoHere
        | GoRight Path

    Show Path where
        show (GoLeft p)  = "l " ++ show p
        show GoHere      = ""
        show (GoRight p) = "r " ++ show p

    parse : List String -> Either String Path
    parse ("l" :: rest) = map GoLeft $ parse rest
    parse []            = pure GoHere
    parse ("r" :: rest) = map GoRight $ parse rest
    parse other         = throw $ "!! '" ++ unwords other ++ "' is not a valid path, type 'help' for more info"


-- Events ----------------------------------------------------------------------

data Action : Type where
    Change   : Universe.typeOf b -> Action
    Empty    : Action
    Pick     : Path -> Action
    Continue : Maybe Path -> Action

data Event
    = ToFirst Event
    | ToThis Action
    | ToSecond Event


-- Showing ---------------------------------------------------------------------

Show Action where
    show (Change _)          = "change <val>"
    show Empty               = "empty"
    show (Pick p)            = "pick " ++ show p
    show (Continue Nothing)  = "cont"
    show (Continue (Just p)) = "cont " ++ show p

Show Event where
    show (ToFirst e)  = "f " ++ show e
    show (ToThis a)   = show a
    show (ToSecond e) = "s " ++ show e


-- Parsing ---------------------------------------------------------------------

usage : String
usage = unlines
    [ ":: Possible events are:"
    , "    change <val> : change current editor to <val> "
    , "    empty        : empty current editor"
    , "    pick <path>  : choose amongst first or second option"
    , "    cont         : continue with the next task"
    , "    cont <path>  : continue with one of possible options"
    , "    f <event>    : send <event> to the first task"
    , "    s <event>    : send <event> to the second task"
    , "    help         : show this message"
    ]

parse : List String -> Either String Event
parse ["change", val] with (Universe.Basic.parse val)
  parse ["change", val] | Nothing          = throw $ "!! Error parsing value '" ++ val ++ "'"
  parse ["change", val] | (Just (ty ** v)) = pure $ ToThis $ Change {b = BasicTy ty} v
parse ["empty"]                            = pure $ ToThis $ Empty
parse ("pick" :: rest)                     = map (ToThis . Pick) $ Path.parse rest
parse ["cont"]                             = pure $ ToThis $ Continue $ Nothing
parse ("cont" :: rest)                     = map (ToThis . Continue . Just) $ Path.parse rest
parse ("f" :: rest)                        = map ToFirst $ parse rest
parse ("s" :: rest)                        = map ToSecond $ parse rest
parse ["help"]                             = throw usage
parse []                                   = throw ""
parse other                                = throw $ "!! '" ++ unwords other ++ "' is not a valid command, type 'help' for more info"
