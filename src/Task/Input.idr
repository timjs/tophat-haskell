module Task.Input


import Control.Catchable

import public Data.Surely

import Task.Universe
import Helpers


%default total
%access export


infix 6 =~, /~



-- Paths -----------------------------------------------------------------------


namespace Path


  public export
  data Path
    = GoLeft Path
    | GoHere
    | GoRight Path


  Eq Path where
    (GoLeft x)  == (GoLeft y)  = x == y
    (GoHere)    == (GoHere)    = True
    (GoRight x) == (GoRight y) = x == y
    _           == _           = False


  Show Path where
    show (GoLeft p)  = " l" ++ show p
    show (GoHere)    = ""
    show (GoRight p) = " r" ++ show p


  parse : List String -> Either String Path
  parse ("l" :: rest) = map GoLeft $ parse rest
  parse []            = ok GoHere
  parse ("r" :: rest) = map GoRight $ parse rest
  parse other         = throw $ "!! `" ++ unwords other ++ "` is not a valid path, type `help` for more info"



-- Events ----------------------------------------------------------------------


public export
data Action : Type where
  Change       : {auto p : IsBasic c} -> {auto eq : Eq (typeOf c)} -> Surely (typeOf c) -> Action
  Empty        : Action
  Pick         : Path -> Action
  PickWith     : Label -> Action
  Continue     : Action
  ContinueWith : Label -> Action


Eq Action where
  (Change {c=c1} x) == (Change {c=c2} y) with (decEq c1 c2)
    (Change {c=c2} x) == (Change {c=c2} y) | Yes Refl = x == y
    (Change {c=c1} x) == (Change {c=c2} y) | No _     = False
  (Empty)           == (Empty)                        = True
  (Pick x)          == (Pick y)                       = x == y
  (PickWith x)      == (PickWith y)                   = x == y
  (Continue)        == (Continue)                     = True
  (ContinueWith x)  == (ContinueWith y)               = x == y
  _                 == _                              = False


public export
data Input
  = ToLeft Input
  | ToHere Action
  | ToRight Input


Eq Input where
  (ToLeft x)  == (ToLeft y)  = x == y
  (ToHere x)  == (ToHere y)  = x == y
  (ToRight x) == (ToRight y) = x == y
  _           == _           = False



-- Conformance -----------------------------------------------------------------


strip : (i : Input) -> Maybe (c : Ty ** (Eq (typeOf c), typeOf c))
strip (ToHere (Change {eq} {c} (Exactly v))) = Just (c ** (eq, v))
strip (ToHere (Change Anything))             = Nothing
strip (ToHere _)                             = Nothing
strip (ToLeft i)                             = strip i
strip (ToRight i)                            = strip i


(=~) : Input -> Input -> Bool
--FIXME: Why does a with-view not work?
i1 =~ i2 = case ( strip i1, strip i2 ) of
  ( Just (c1 ** (eq1, v1)), Just (c2 ** (eq2, v2)) ) => case (decEq c1 c2) of
    Yes Refl => v1 == v2
    No contr => False
  _ => True


(/~) : Input -> Input -> Bool
i1 /~ i2 = not (i1 =~ i2)



-- Showing ---------------------------------------------------------------------


Show Action where
  show (Change _)       = "change <val>"
  show (Empty)          = "empty"
  show (Pick p)         = "pick" ++ show p
  show (PickWith l)     = "pick " ++ l
  show (Continue)       = "cont"
  show (ContinueWith l) = "cont " ++ l


Show Input where
  show (ToLeft e)  = "l " ++ show e
  show (ToHere a)   = show a
  show (ToRight e) = "r " ++ show e



-- Parsing ---------------------------------------------------------------------


usage : String
usage = unlines
  [ ":: Possible events are:"
  , "    change <value> : change current editor to <value> "
  , "    empty          : empty current editor"
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


parse : List String -> Either String Input
parse [ "change", val ] with (Universe.parse val)
  | Nothing              = throw $ "!! Error parsing value `" ++ val ++ "`"
  -- NOTE:
  -- `p` is the proof that `IsBasic c`, and `eq` is the dictionary holding `Eq (typeOf c)`.
  -- Both are used by the auto-implicits of the `Change` constructor.
  | Just (c**(p, eq, v)) = ok $ ToHere $ Change {c} (Exactly v)
parse [ "empty" ]        = ok $ ToHere $ Empty
parse [ "pick", next ]   =
  if isLabel next then
    ok $ ToHere $ PickWith next
  else
    map (ToHere . Pick) $ Path.parse [ next ]
parse ("pick" :: rest)   = map (ToHere . Pick) $ Path.parse rest
parse [ "cont" ]         = ok $ ToHere $ Continue
parse [ "cont", next ]   =
  if isLabel next then
    ok $ ToHere $ ContinueWith next
  else
    throw $ "!! Could not parse `" ++ next ++ "` as a label, type `help` for more info"
parse ("l" :: rest)      = map ToLeft $ parse rest
parse ("r" :: rest)      = map ToRight $ parse rest
parse [ "help" ]         = throw usage
parse other              = throw $ "!! `" ++ unwords other ++ "` is not a valid command, type `help` for more info"
