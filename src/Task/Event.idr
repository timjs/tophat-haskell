module Task.Event

import Control.Catchable

import Task.Universe

%default total
%access public export


-- Labels ----------------------------------------------------------------------

Label : Type
Label = String

isLabel : String -> Bool
isLabel s               with ( strM s )
 isLabel ""             | StrNil      = False
 isLabel (strCons c cs) | StrCons c _ = isUpper c


-- Paths -----------------------------------------------------------------------

namespace Path

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
  parse []            = pure GoHere
  parse ("r" :: rest) = map GoRight $ parse rest
  parse other         = throw $ "!! '" ++ unwords other ++ "' is not a valid path, type 'help' for more info"


-- Events ----------------------------------------------------------------------

data Action
  = Change (Universe.typeOf b)
  | Clear
  | Pick (Either Label Path)
  | Continue (Maybe Label)

data Event
  = ToLeft Event
  | ToHere Action
  | ToRight Event


-- Showing ---------------------------------------------------------------------

Show Action where
  show (Change _)          = "change <val>"
  show (Clear)             = "clear"
  show (Pick (Left l))     = "pick " ++ l
  show (Pick (Right p))    = "pick" ++ show p
  show (Continue Nothing)  = "cont"
  show (Continue (Just l)) = "cont " ++ l

Show Event where
  show (ToLeft e)  = "l " ++ show e
  show (ToHere a)   = show a
  show (ToRight e) = "r " ++ show e


-- Parsing ---------------------------------------------------------------------

usage : String
usage = unlines
  [ ":: Possible events are:"
  , "    change <val> : change current editor to <val> "
  , "    clear        : clear current editor"
  , "    pick <path>  : choose amongst the possible options"
  , "    pick <label> : choose amongst the possible options"
  , "    cont         : continue with the next task"
  , "    cont <label> : continue with one of possible options"
  , "    l <event>    : send <event> to the left task"
  , "    r <event>    : send <event> to the right task"
  , "    help         : show this message"
  , ""
  , "where values <val> can be:"
  , "    ()          (Unit)"
  , "    True, False (Booleans)"
  , "    1, 32, -42  (Integers)"
  , "    \"Hello\"   (Strings)"
  ]

parse : List String -> Either String Event
parse ["change", val] with (Universe.Basic.parse val)
  | Nothing            = throw $ "!! Error parsing value '" ++ val ++ "'"
  | (Just (ty ** v))   = pure $ ToHere $ Change {b = BasicTy ty} v
parse ["clear"]        = pure $ ToHere $ Clear
parse ["pick", label] with ( isLabel label )
  | True               = pure $ ToHere $ Pick $ Left label
  | False              = map (ToHere . Pick . Right) $ Path.parse [label]
parse ("pick" :: rest) = map (ToHere . Pick . Right) $ Path.parse rest
parse ["cont"]         = pure $ ToHere $ Continue Nothing
parse [ "cont", label] = pure $ ToHere $  Continue $ Just label
parse ("l" :: rest)    = map ToLeft $ parse rest
parse ("r" :: rest)    = map ToRight $ parse rest
parse ["help"]         = throw usage
parse []               = throw ""
parse other            = throw $ "!! '" ++ unwords other ++ "' is not a valid command, type 'help' for more info"
