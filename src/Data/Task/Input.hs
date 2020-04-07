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

instance Eq Action where
  IEnter x == IEnter y
    | Just Refl <- x ~= y = x == y
    | otherwise = False
  ISelect x == ISelect y = x == y
  _ == _ = False

instance Pretty Action where
  pretty = \case
    IEnter x -> pretty x
    ISelect l -> pretty l

-- Dummy actions --

data Dummy :: Type where
  AEnter :: Editable b => Proxy b -> Dummy
  ASelect :: Label -> Dummy

instance Eq Dummy where
  AEnter x == AEnter y
    -- NOTE: We're comparing proxies, they are always equal when the types are equal.
    | Just Refl <- x ~= y = True
    | otherwise = False
  ASelect x == ASelect y = x == y
  _ == _ = False

instance Pretty Dummy where
  pretty = \case
    AEnter _ -> "<value>"
    ASelect l -> pretty l

-- Symbolic actions --

data Symbolic :: Type where
  SEnter :: Editable b => Proxy b -> Symbolic
  SSelect :: Label -> Symbolic

-- Inputs ----------------------------------------------------------------------

data Input a
  = Input Nat a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Input a) where
  pretty = \case
    Input n a -> cat ["@", pretty n, " ", pretty a]

-- Conformance -----------------------------------------------------------------

dummyfy :: Action -> Dummy
dummyfy = \case
  IEnter x -> AEnter (proxyOf x)
  ISelect l -> ASelect l

-- reify :: Dummy -> Gen (List Action)
-- reify (AEnter l)  = map IEnter <$> vectorOf 5 (arbitraryOf l)
-- reify (ASelect l)   = pure [ ISelect l ]

strip :: Input Action -> Input Dummy
strip = map dummyfy

-- fill :: Input Dummy -> Gen (List (Input Action))
-- fill = map sequence << sequence << map reify

-- Parsing ---------------------------------------------------------------------

usage :: Doc n
usage =
  split
    [ ":: Possible inputs are:",
      "    <id> <value> : enter <value> into the current editor",
      "    <id> <label> : select one of the possible options",
      "    help         : show this message",
      "    quit         : quit",
      "",
      "where ids have the form:",
      "    @2, @37, …",
      "",
      "and values can be:",
      "    ()           : Unit",
      "    True, False  : Booleans",
      -- "    +0, +1, …    : Naturals",
      "    1, -42, …    : Integers",
      "    \"Hello\", …   : Strings",
      "    [ <value>, ] : List of values",
      "",
      "and labels _always_ start with a Capital letter"
    ]

parseId :: Text -> Either (Doc n) Nat
parseId t
  | Just (c, n) <- Text.uncons t,
    c == '@',
    Just v <- scan n :: Maybe Nat =
    ok v
  | otherwise = throw <| sep ["!!", dquotes <| pretty t, "is not a proper id"]

parseLabel :: Text -> Either (Doc n) Action
parseLabel t
  | Just (c, _) <- Text.uncons t, Char.isUpper c = ok <| ISelect t
  | otherwise = throw <| sep ["!!", dquotes <| pretty t, "is not a proper label"]

parseVal :: Text -> Either (Doc n) Action
parseVal val
  | Just v <- scan val :: Maybe Unit = ok <| IEnter v
  | Just v <- scan val :: Maybe Bool = ok <| IEnter v
  | Just v <- scan val :: Maybe Int = ok <| IEnter v
  | Just v <- scan val :: Maybe Double = ok <| IEnter v
  | Just v <- scan val :: Maybe Text = ok <| IEnter v
  | Just v <- scan val :: Maybe [Bool] = ok <| IEnter v
  | Just v <- scan val :: Maybe [Int] = ok <| IEnter v
  | Just v <- scan val :: Maybe [Double] = ok <| IEnter v
  | Just v <- scan val :: Maybe [Text] = ok <| IEnter v
  | otherwise = throw <| sep ["!! Error parsing value", dquotes (pretty val)]

parse :: List Text -> Either (Doc a) (Input Action)
parse (x : i : _)
  | Right v <- parseVal x,
    Right n <- parseId i =
    ok <| Input n v
  | Right l <- parseLabel x,
    Right n <- parseId i =
    ok <| Input n l
parse ["h"] = throw usage
parse ["help"] = throw usage
parse other = throw <| sep ["!!", dquotes (sep <| map pretty other), "is not a valid command, type `help` for more info"]
