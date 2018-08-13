module Task.Event


import Control.Catchable

import Task.Universe
import Helpers


%default total
%access export


-- Paths -----------------------------------------------------------------------

namespace Path

  public export
  data Path
    = GoLeft Path
    | GoHere
    | GoRight Path

  Show Path where
    show (GoLeft p)  = " l" ++ show p
    show GoHere      = ""
    show (GoRight p) = " r" ++ show p

  parse : List String -> Either String Path
  parse ("l" :: rest) = map GoLeft $ parse rest
  parse []            = ok GoHere
  parse ("r" :: rest) = map GoRight $ parse rest
  parse other         = throw $ "!! '" ++ unwords other ++ "' is not a valid path, type 'help' for more info"


-- Events ----------------------------------------------------------------------

||| Note:
||| - The `Nothing` case for `Change` is used as a dummy when calculation possible events.
public export
data Action : Type where
  Change   : {auto p : IsBasic c} -> Maybe (typeOf c) -> Action
  Clear    : Action
  Pick     : Path -> Action
  PickAt   : Label -> Action
  Continue : Maybe Label -> Action

public export
data Event
  = ToLeft Event
  | ToHere Action
  | ToRight Event


-- Showing ---------------------------------------------------------------------

Show Action where
  show (Change _)          = "change <val>"
  show (Clear)             = "clear"
  show (Pick p)            = "pick" ++ show p
  show (PickAt l)          = l
  show (Continue Nothing)  = "cont"
  show (Continue (Just l)) = l

Show Event where
  show (ToLeft e)  = "l " ++ show e
  show (ToHere a)   = show a
  show (ToRight e) = "r " ++ show e


-- Parsing ---------------------------------------------------------------------

usage : String
usage = unlines
  [ ":: Possible events are:"
  , "    change <value> : change current editor to <value> "
  , "    clear          : clear current editor"
  , "    pick <path>    : choose amongst the possible options"
  , "    cont           : continue with the next task"
  , "    <label>        : continue with the labeled task"
  , "    l <event>      : send <event> to the left task"
  , "    r <event>      : send <event> to the right task"
  , "    help           : show this message"
  , ""
  , "where values can be:"
  , "    ()          : Unit"
  , "    True, False : Booleans"
  , "    1, 32, -42  : Integers"
  , "    \"Hello\"   : Strings"
  , ""
  , "paths are of the form:"
  , "   l <path>? : go left"
  , "   r <path>? : go right"
  , ""
  , "and labels always start with a Capital letter"
  ]

mutual
  parse : List String -> Either String Event
  parse (first :: rest) =
    if isLabel first then
      ok $ ToHere $ PickAt first
    else
      parse' (first :: rest)
  parse []  = throw ":: Please enter a command or label, type `help` for more info"

  parse' : List String -> Either String Event
  parse' ["change", val] with (Universe.parse val)
    | Nothing             = throw $ "!! Error parsing value '" ++ val ++ "'"
    | Just (c ** ( p, v ))= ok $ ToHere $ Change {c} (Just v)
  parse' ["clear"]        = ok $ ToHere $ Clear
  parse' ("pick" :: rest) = map (ToHere . Pick) $ Path.parse rest
  parse' ["cont"]         = ok $ ToHere $ Continue Nothing
  parse' [ "cont", label] = ok $ ToHere $  Continue $ Just label
  parse' ("l" :: rest)    = map ToLeft $ parse rest
  parse' ("r" :: rest)    = map ToRight $ parse rest
  parse' ["help"]         = throw usage
  parse' other            = throw $ "!! `" ++ unwords other ++ "` is not a valid command, type `help` for more info"
