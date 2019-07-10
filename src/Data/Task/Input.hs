module Data.Task.Input
  ( Path(..), Action(..), Dummy(..), Symbolic(..), Input(..)
  , dummyfy, strip
  , usage, parse
  ) where


import Data.Editable



-- Paths -----------------------------------------------------------------------


data Path
  = GoLeft Path
  | GoHere
  | GoRight Path
  deriving ( Eq, Show )


instance Pretty Path where
  pretty = \case
    GoLeft p  -> sep [ "l", pretty p ]
    GoHere    -> ""
    GoRight p -> sep [ "r", pretty p ]


parsePath :: List Text -> Either (Doc a) Path
parsePath = \case
  "l" : rest -> map GoLeft $ parsePath rest
  []         -> ok GoHere
  "r" : rest -> map GoRight $ parsePath rest
  other      -> throw $ sep [ "!!", dquotes (pretty other), "is not a valid path, type `help` for more info" ]



-- Actions ---------------------------------------------------------------------

-- Real actions --


data Action :: Type where
  IChange   :: Editable b => b -> Action
  IPick     :: Path -> Action
  IContinue :: Path -> Action


instance Eq Action where
  IChange x   == IChange y
    | Just Refl <- x ~= y    = x == y
    | otherwise              = False
  IPick x     == IPick y     = x == y
  IContinue x == IContinue y = x == y
  _           == _           = False


instance Pretty Action where
  pretty = \case
    IChange x   -> sep [ "change", pretty x ]
    IPick p     -> sep [ "pick", pretty p ]
    IContinue p -> sep [ "cont", pretty p ]



-- Dummy actions --


data Dummy :: Type where
  AChange   :: Editable b => Proxy b -> Dummy
  APick     :: Path -> Dummy
  AContinue :: Path -> Dummy


instance Eq Dummy where
  AChange x   == AChange y
    -- NOTE: We're comparing proxies, they are always equal when the types are equal.
    | Just Refl <- x ~= y    = True
    | otherwise              = False
  APick x     == APick y     = x == y
  AContinue x == AContinue y = x == y
  _           == _           = False


instance Pretty Dummy where
  pretty = \case
    AChange _   -> "change <val>"
    APick p     -> sep [ "pick", pretty p ]
    AContinue p -> sep [ "cont", pretty p ]



-- Symbolic actions --


data Symbolic :: Type where
  SChange :: Editable b => Proxy b -> Symbolic



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
  IChange x   -> AChange (proxyOf x)
  IPick p     -> APick p
  IContinue p -> AContinue p


-- reify :: Dummy -> Gen (List Action)
-- reify (AChange p) = map IChange <$> vectorOf 5 (arbitraryOf p)
-- reify (APick p)   = pure [ IPick p ]


strip :: Input Action -> Input Dummy
strip = map dummyfy

-- fill :: Input Dummy -> Gen (List (Input Action))
-- fill = map sequence << sequence << map reify



-- Parsing ---------------------------------------------------------------------


usage :: Doc a
usage = cat
  [ ":: Possible inputs are:"
  , "    change <value> : change current editor to <value> "
  , "    pick <path>    : pick amongst the possible options"
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
  | Just v <- scan val :: Maybe Unit      = ok $ ToHere $ IChange v
  | Just v <- scan val :: Maybe Bool      = ok $ ToHere $ IChange v
  | Just v <- scan val :: Maybe Int       = ok $ ToHere $ IChange v
  | Just v <- scan val :: Maybe String    = ok $ ToHere $ IChange v
  | Just v <- scan val :: Maybe [Bool]    = ok $ ToHere $ IChange v
  | Just v <- scan val :: Maybe [Int]     = ok $ ToHere $ IChange v
  | Just v <- scan val :: Maybe [String]  = ok $ ToHere $ IChange v
  | otherwise                             = throw $ sep [ "!! Error parsing value", dquotes (pretty val) ]
parse ("pick" : next)                     = map (ToHere << IPick) $ parsePath next
parse ("f" : rest)                        = map ToFirst $ parse rest
parse ("s" : rest)                        = map ToSecond $ parse rest
parse [ "help" ]                          = throw usage
parse other                               = throw $ sep [ "!!", dquotes (sep $ map pretty other), "is not a valid command, type `help` for more info" ]
