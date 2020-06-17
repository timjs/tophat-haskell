module Task.Input
  ( Concrete (..),
    Symbolic (..),
    Dummy,
    dummy,
    Input (..),
    pattern ISelect,
    pattern IPreselect,
    Option (..),
    fromOption,
    usage,
    parse,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Task (Basic, Label, Name (..))

---- Actions -------------------------------------------------------------------

---- Concrete actions

data Concrete :: Type where
  Concrete :: Basic b => b -> Concrete

instance Eq Concrete where
  Concrete x == Concrete y
    | Just Refl <- x ~= y = x == y
    | otherwise = False

instance Display Concrete where
  display = \case
    Concrete x -> display x

---- Symbolic actions

data Symbolic :: Type where
  Symbolic :: Basic b => Proxy b -> Symbolic

instance Eq Symbolic where
  Symbolic x == Symbolic y
    -- NOTE: We're comparing proxies, they are always equal when the types are equal.
    | Just Refl <- x ~= y = True
    | otherwise = False

instance Display Symbolic where
  display = \case
    Symbolic p -> concat ["<", display beta, ">"]
      where
        beta = typeOfProxy p

---- Dummy actions

type Dummy = Symbolic

dummy :: Basic b => Proxy b -> Dummy
dummy p = Symbolic p

---- Inputs --------------------------------------------------------------------

data Input b
  = IEnter Nat b
  | IOption Name Label
  deriving (Eq, Debug, Functor, Foldable, Traversable)

{-# COMPLETE IEnter, ISelect, IPreselect #-}

pattern ISelect :: Nat -> Label -> Input b
pattern ISelect n l = IOption (Named n) l

pattern IPreselect :: Label -> Input b
pattern IPreselect l = IOption Unnamed l

instance Display b => Display (Input b) where
  display = \case
    IEnter n b -> unwords [display n, display b]
    ISelect n l -> unwords [display n, display l]
    IPreselect l -> display l

---- Action view

-- data Action b
--   = AValue b
--   | ALabel Label

-- {-# COMPLETE ISend, IPreselect #-}
-- pattern ISend :: Nat -> Action b -> Input b
-- pattern ISend n a <- (action -> Just (n, a))

-- action:: Input b -> Maybe (Nat, Action b)
-- action= \case
--   IEnter n b -> Just (n, AValue b)
--   ISelect n l -> Just (n, ALabel l)
--   IPreselect _ -> Nothing

---- Options -------------------------------------------------------------------

data Option
  = Option Name Label
  deriving (Eq, Ord, Debug, Scan)

instance Display Option where
  display (Option n l) = concat [display l, "^", display n]

fromOption :: Option -> Input b
fromOption (Option n l) = IOption n l

---- Conformance ---------------------------------------------------------------

-- dummyfy :: Action -> Dummy
-- dummyfy = \case
--   IEnter x -> AEnter (proxyOf x)
--   ISelect l -> ASelect l

-- reify :: Dummy -> Gen (List Action)
-- reify (AEnter l)  = map IEnter <$> vectorOf 5 (arbitraryOf l)
-- reify (ASelect l)   = pure [ ISelect l ]

-- strip :: Input Action -> Input Dummy
-- strip = map dummyfy

-- fill :: Input Dummy -> Gen (List (Input Action))
-- fill = map sequence << sequence << map reify

---- Parsing -------------------------------------------------------------------

usage :: Text
usage =
  unlines
    [ ":: Possible inputs are:",
      "    <id> <value> : enter <value> into editor <id>",
      "    <id> <label> : select one of the possible options from editor <id>",
      "    <label>      : continue with one of the possible options",
      "    help, h      : show this message",
      "    quit, q      : quit",
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

parseId :: Text -> Either Text Nat
parseId t
  | Just v <- scan t :: Maybe Nat = ok v
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper id"]

parseLabel :: Text -> Either Text Label
parseLabel t
  | Just (c, _) <- Text.uncons t, Char.isUpper c = ok <| t
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper label"]

parseConcrete :: Text -> Either Text Concrete
parseConcrete val
  | Just v <- scan val :: Maybe Unit = ok <| Concrete v
  | Just v <- scan val :: Maybe Bool = ok <| Concrete v
  | Just v <- scan val :: Maybe Int = ok <| Concrete v
  | Just v <- scan val :: Maybe Double = ok <| Concrete v
  | Just v <- scan val :: Maybe Text = ok <| Concrete v
  | Just v <- scan val :: Maybe [Bool] = ok <| Concrete v
  | Just v <- scan val :: Maybe [Int] = ok <| Concrete v
  | Just v <- scan val :: Maybe [Double] = ok <| Concrete v
  | Just v <- scan val :: Maybe [Text] = ok <| Concrete v
  | otherwise = error <| unwords ["!! Error parsing value", display val |> quote]

parse :: Text -> Either Text (Input Concrete)
parse t = case Text.words t of
  ["help"] -> error usage
  ["h"] -> error usage
  [i, x] -> do
    n <- parseId i
    map (ISelect n) (parseLabel x) ++ map (IEnter n) (parseConcrete x) --NOTE: should be `<|>`, but we've got some strange import of `Error` getting in the way
  [x] -> do
    l <- parseLabel x
    ok <| IPreselect l
  _ -> error <| unwords ["!!", display t |> quote, "is not a valid command, type `help` for more info"]
