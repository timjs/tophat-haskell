module Data.Task.Input
  ( Path(..), Action(..), Dummy(..), Symbolic(..), Input(..)
  , dummyfy, strip
  , usage, parse
  ) where


import Data.Task



-- Paths -----------------------------------------------------------------------


data Path
  = GoLeft
  | GoRight
  deriving ( Eq, Show )


instance Pretty Path where
  pretty GoLeft  = "l"
  pretty GoRight = "r"


parsePath :: Text -> Either (Doc a) Path
parsePath = \case
  "l"   -> ok GoLeft
  "r"   -> ok GoRight
  other -> throw $ sep [ "!!", dquotes (pretty other), "is not a valid path, type `help` for more info" ]



-- Actions ---------------------------------------------------------------------

-- Real actions --


data Action :: Type where
  Change   :: Basic b => b -> Action
  Empty    :: Action
  Pick     :: Path -> Action
  Continue :: Action


instance Eq Action where
  Change x == Change y
    | Just Refl <- x ~= y = x == y
    | otherwise           = False
  Empty    == Empty       = True
  Pick x   == Pick y      = x == y
  Continue == Continue    = True
  _        == _           = False


instance Pretty Action where
  pretty = \case
    Change x -> sep [ "change", pretty x ]
    Empty    -> "empty"
    Pick p   -> sep [ "pick", pretty p ]
    Continue -> "cont"



-- Dummy actions --


data Dummy :: Type where
  AChange       :: Basic b => Proxy b -> Dummy
  AEmpty        :: Dummy
  APick         :: Path -> Dummy
  AContinue     :: Dummy


instance Eq Dummy where
  AChange x == AChange y
    -- NOTE: We're comparing proxies, they are always equal when the types are equal.
    | Just Refl <- x ~= y = True
    | otherwise           = False
  AEmpty    == AEmpty     = True
  APick x   == APick y    = x == y
  AContinue == AContinue  = True
  _         == _          = False


instance Pretty Dummy where
  pretty = \case
    AChange _ -> "change <val>"
    AEmpty    -> "empty"
    APick p   -> sep [ "pick", pretty p ]
    AContinue -> "cont"



-- Symbolic actions --


data Symbolic :: Type where
  SChange :: Basic b => Proxy b -> Symbolic



-- Inputs ----------------------------------------------------------------------


data Input a
  = ToFirst (Input a)
  | ToHere a
  | ToSecond (Input a)
  deriving ( Eq, Show, Functor, Foldable, Traversable )


instance Pretty a => Pretty (Input a) where
  pretty = \case
    ToFirst e  -> sep [ "f", pretty e ]
    ToHere a   -> pretty a
    ToSecond e -> sep [ "s", pretty e ]



-- Conformance -----------------------------------------------------------------


dummyfy :: Action -> Dummy
dummyfy = \case
  Change x -> AChange (proxyOf x)
  Empty    -> AEmpty
  Pick p   -> APick p
  Continue -> AContinue


-- reify :: Dummy -> Gen (List Action)
-- reify (AChange p) = map Change <$> vectorOf 5 (arbitraryOf p)
-- reify (AEmpty)    = pure [ Empty ]
-- reify (APick p)   = pure [ Pick p ]
-- reify (AContinue) = pure [ Continue ]


strip :: Input Action -> Input Dummy
strip = map dummyfy

-- fill :: Input Dummy -> Gen (List (Input Action))
-- fill = map sequence << sequence << map reify



-- Parsing ---------------------------------------------------------------------


usage :: Doc a
usage = vcat
  [ ":: Possible inputs are:"
  , "    change <value> : change current editor to <value> "
  , "    empty          : empty current editor"
  , "    pick <path>    : pick amongst the possible options"
  , "    cont           : continue with the next task"
  , "    f <input>      : send <input> to the first task"
  , "    s <input>      : send <input> to the second task"
  , "    help           : show this message"
  , ""
  , "where values can be:"
  , "    ()          : Unit"
  , "    True, False : Booleans"
  , "    1, 32, -42  : Integers"
  , "    \"Hello\"   : Strings"
  , ""
  , "paths are of the form:"
  , "   l : go left"
  , "   r : go right"
  ]


parse :: List Text -> Either (Doc a) (Input Action)
parse [ "change", val ]
  | Just v <- read val :: Maybe Unit      = ok $ ToHere $ Change v
  | Just v <- read val :: Maybe Bool      = ok $ ToHere $ Change v
  | Just v <- read val :: Maybe Int       = ok $ ToHere $ Change v
  | Just v <- read val :: Maybe String    = ok $ ToHere $ Change v
  | Just v <- read val :: Maybe [Bool]    = ok $ ToHere $ Change v
  | Just v <- read val :: Maybe [Int]     = ok $ ToHere $ Change v
  | Just v <- read val :: Maybe [String]  = ok $ ToHere $ Change v
  | otherwise                             = throw $ sep [ "!! Error parsing value", dquotes (pretty val) ]
parse [ "empty" ]                         = ok $ ToHere Empty
parse [ "pick", next ]                    = map (ToHere << Pick) $ parsePath next
parse [ "cont" ]                          = ok $ ToHere Continue
parse ("f" : rest)                        = map ToFirst $ parse rest
parse ("s" : rest)                        = map ToSecond $ parse rest
parse [ "help" ]                          = throw usage
parse other                               = throw $ sep [ "!!", dquotes (sep $ map pretty other), "is not a valid command, type `help` for more info" ]
