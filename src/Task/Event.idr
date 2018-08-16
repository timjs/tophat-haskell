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
  Change       : {auto p : IsBasic c} -> Maybe (typeOf c) -> Action
  Clear        : Action
  Pick         : Path -> Action
  PickWith     : Label -> Action
  Continue     : Action
  ContinueWith : Label -> Action


public export
data Event
  = ToLeft Event
  | ToHere Action
  | ToRight Event



-- Showing ---------------------------------------------------------------------


Show Action where
  show (Change _)       = "change <val>"
  show (Clear)          = "clear"
  show (Pick p)         = "pick" ++ show p
  show (PickWith l)       = "pick " ++ l
  show (Continue)       = "cont"
  show (ContinueWith l) = "cont " ++ l


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
  , "    pick <path>    : pick amongst the possible options"
  , "    pick <label>   : pick the option labeld with <label>"
  , "    cont           : continue with the next task"
  , "    cont <label>   : continue with the task labeld <label>"
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


parse : List String -> Either String Event
parse [ "change", val ] with (Universe.parse val)
  | Nothing              = throw $ "!! Error parsing value '" ++ val ++ "'"
  | Just (c ** ( p, v )) = ok $ ToHere $ Change {c} (Just v)
parse [ "clear" ]        = ok $ ToHere $ Clear
parse [ "pick", next ]   =
  if isLabel next then
    ok $ ToHere $ PickWith next
  else
    map (ToHere . Pick) $ Path.parse [ next ]
parse ("pick" :: rest)   = map (ToHere . Pick) $ Path.parse rest
parse [ "cont" ]         = ok $ ToHere $ Continue
parse [ "cont", next ]  =
  if isLabel next then
    ok $ ToHere $ ContinueWith next
  else
    throw $ "!! Could not parse `" ++ next ++ "` as a label, type `help` for more info"
parse ("l" :: rest)      = map ToLeft $ parse rest
parse ("r" :: rest)      = map ToRight $ parse rest
parse [ "help" ]         = throw usage
parse other              = throw $ "!! `" ++ unwords other ++ "` is not a valid command, type `help` for more info"
