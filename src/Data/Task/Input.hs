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
import qualified Data.Text.Prettyprint.Doc as Pretty

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
    AEnter p -> cat ["<", pretty beta, ">"]
      where
        beta = typeOfProxy p
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
    Input n a -> cat [pretty a, "@", pretty n]

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
      "    <value>@<id> : enter <value> into the current editor",
      "    <label>@<id> : select one of the possible options",
      "    h(elp)       : show this message",
      "    q(uit)       : quit",
      "",
      "where ids have the form:",
      "    2, 37, …",
      "",
      "values can be:",
      "    ()           : Unit",
      "    True, False  : Booleans",
      -- "    +0, +1, …    : Naturals",
      "    1, -42, …    : Integers",
      "    \"Hello\", …   : Strings",
      "    [ <value>, ] : List of values",
      "",
      "and labels:",
      "    Start With A Capital Letter"
    ]

parseId :: Text -> Either (Doc n) Nat
parseId t
  | Just v <- scan t :: Maybe Nat = ok v
  | otherwise = throw <| sep ["!!", Pretty.dquotes <| pretty t, "is not a proper id"]

parseLabel :: Text -> Either (Doc n) Action
parseLabel t
  | Just (c, _) <- Text.uncons t, Char.isUpper c = ok <| ISelect t
  | otherwise = throw <| sep ["!!", Pretty.dquotes <| pretty t, "is not a proper label"]

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
  | otherwise = throw <| sep ["!! Error parsing value", Pretty.dquotes (pretty val)]

parse :: Text -> Either (Doc a) (Input Action)
parse t = case Text.splitOn "@" t of
  [x, i] -> do
    n <- parseId i
    r <- parseVal x ++ parseLabel x --XXX: should be `<|>`, but we've got some strange import of `Error` getting in the way
    ok <| Input n r
  ["help"] -> throw usage
  ["h"] -> throw usage
  _ -> throw <| sep ["!!", Pretty.dquotes (pretty t), "is not a valid command, type `help` for more info"]
