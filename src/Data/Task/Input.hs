module Data.Task.Input
  ( Action (..),
    Dummy (..),
    Symbolic (..),
    Input (..),
    dummyfy,
    strip,
    usage,
    parse,
  )
where

import qualified Data.Char as Char
import Data.Task
import qualified Data.Text as Text

-- Actions ---------------------------------------------------------------------

-- Real actions --

data Action :: Type where
  IEnter :: Editable b => b -> Action
  ISelect :: Label -> Action
  IContinue :: Label -> Action

instance Eq Action where
  IEnter x == IEnter y
    | Just Refl <- x ~= y = x == y
    | otherwise = False
  ISelect x == ISelect y = x == y
  IContinue x == IContinue y = x == y
  _ == _ = False

instance Pretty Action where
  pretty = \case
    IEnter x -> sep ["e", pretty x]
    ISelect l -> sep ["p", pretty l]
    IContinue l -> sep ["c", pretty l]

-- Dummy actions --

data Dummy :: Type where
  AEnter :: Editable b => Proxy b -> Dummy
  ASelect :: Label -> Dummy
  AContinue :: Label -> Dummy

instance Eq Dummy where
  AEnter x == AEnter y
    -- NOTE: We're comparing proxies, they are always equal when the types are equal.
    | Just Refl <- x ~= y = True
    | otherwise = False
  ASelect x == ASelect y = x == y
  AContinue x == AContinue y = x == y
  _ == _ = False

instance Pretty Dummy where
  pretty = \case
    AEnter _ -> sep ["e", "<value>"]
    ASelect l -> sep ["p", pretty l]
    AContinue l -> sep ["c", pretty l]

-- Symbolic actions --

data Symbolic :: Type where
  SChange :: Editable b => Proxy b -> Symbolic

-- Inputs ----------------------------------------------------------------------

data Input a
  = ToFirst (Input a)
  | ToHere a
  | ToSecond (Input a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Input a) where
  pretty = \case
    ToFirst e -> sep ["f", pretty e]
    ToHere a -> pretty a
    ToSecond e -> sep ["s", pretty e]

-- Conformance -----------------------------------------------------------------

dummyfy :: Action -> Dummy
dummyfy = \case
  IEnter x -> AEnter (proxyOf x)
  ISelect l -> ASelect l
  IContinue l -> AContinue l

-- reify :: Dummy -> Gen (List Action)
-- reify (AEnter l)  = map IEnter <$> vectorOf 5 (arbitraryOf l)
-- reify (ASelect l)   = pure [ ISelect l ]

strip :: Input Action -> Input Dummy
strip = map dummyfy

-- fill :: Input Dummy -> Gen (List (Input Action))
-- fill = map sequence << sequence << map reify

-- Parsing ---------------------------------------------------------------------

usage :: Doc a
usage =
  split
    [ ":: Possible inputs are:",
      "    e <value> : enter <value> into the current editor",
      "    p <label> : pick amongst the possible options",
      "    c <label> : continue with a possible option",
      "    f <input> : send <input> to the first task",
      "    s <input> : send <input> to the second task",
      "    help      : show this message",
      "    quit      : quit"
        "",
      "where values can be:",
      "    ()           : Unit",
      "    True, False  : Booleans",
      -- "    0, 1, …      : Naturals",
      "    1, -42, …    : Integers",
      "    \"Hello\", …   : Strings",
      "    [ <value>, ] : List of values",
      "",
      "and labels always start with a Capital letter"
    ]

parseLabel :: Text -> Either (Doc a) Label
parseLabel t
  | Just (c, _) <- Text.uncons t, Char.isUpper c = ok t
  | otherwise = throw <| sep ["!!", dquotes <| pretty t, "is not a proper label"]

parse :: List Text -> Either (Doc a) (Input Action)
parse ["e", val]
  | Just v <- scan val :: Maybe Unit = ok <| ToHere <| IEnter v
  | Just v <- scan val :: Maybe Bool = ok <| ToHere <| IEnter v
  | Just v <- scan val :: Maybe Int = ok <| ToHere <| IEnter v
  | Just v <- scan val :: Maybe Double = ok <| ToHere <| IEnter v
  | Just v <- scan val :: Maybe Text = ok <| ToHere <| IEnter v
  | Just v <- scan val :: Maybe [Bool] = ok <| ToHere <| IEnter v
  | Just v <- scan val :: Maybe [Int] = ok <| ToHere <| IEnter v
  | Just v <- scan val :: Maybe [Double] = ok <| ToHere <| IEnter v
  | Just v <- scan val :: Maybe [Text] = ok <| ToHere <| IEnter v
  | otherwise = throw <| sep ["!! Error parsing value", dquotes (pretty val)]
parse ["p", rest] = map (ToHere << ISelect) <| parseLabel rest
parse ["c", rest] = map (ToHere << IContinue) <| parseLabel rest
parse ("f" : rest) = map ToFirst <| parse rest
parse ("s" : rest) = map ToSecond <| parse rest
parse ["h"] = throw usage
parse ["help"] = throw usage
parse other = throw <| sep ["!!", dquotes (sep <| map pretty other), "is not a valid command, type `help` for more info"]
