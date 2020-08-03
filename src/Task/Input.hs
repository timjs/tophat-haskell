module Task.Input
  ( Concrete (..),
    Symbolic (..),
    Dummy,
    dummy,
    Input (..),
    pattern Pick,
    pattern Prepick,
    usage,
    parse,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Task (Basic, Label, Name (..))

---- Inputs --------------------------------------------------------------------

---- Concrete inputs

data Concrete :: Type where
  Concrete :: Basic b => b -> Concrete

instance Eq Concrete where
  Concrete x == Concrete y
    | Just Refl <- x ~= y = x == y
    | otherwise = False

instance Display Concrete where
  display = \case
    Concrete x -> display x

---- Symbolic inputs

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

---- Dummy inputs

type Dummy = Symbolic

dummy :: Basic b => Proxy b -> Dummy
dummy p = Symbolic p

---- Inputs

data Input b
  = Insert Nat b
  | Option Name Label
  deriving (Eq, Debug, Functor, Foldable, Traversable)

{-# COMPLETE Insert, Pick, Prepick #-}

pattern Pick :: Nat -> Label -> Input b
pattern Pick n l = Option (Named n) l

pattern Prepick :: Label -> Input b
pattern Prepick l = Option Unnamed l

instance Display b => Display (Input b) where
  display = \case
    Insert n b -> unwords [display n, display b]
    Pick n l -> unwords [display n, display l]
    Prepick l -> display l

---- Action view

-- data Action b
--   = AValue b
--   | ALabel Label

-- {-# COMPLETE ISend, Prepick #-}
-- pattern ISend :: Nat -> Action b -> Input b
-- pattern ISend n a <- (action -> Just (n, a))

-- action:: Input b -> Maybe (Nat, Action b)
-- action= \case
--   Insert n b -> Just (n, AValue b)
--   Pick n l -> Just (n, ALabel l)
--   Prepick _ -> Nothing

---- Conformance ---------------------------------------------------------------

-- dummyfy :: Action -> Dummy
-- dummyfy = \case
--   Insert x -> AEnter (proxyOf x)
--   Pick l -> ASelect l

-- reify :: Dummy -> Gen (List Action)
-- reify (AEnter l)  = map Insert <$> vectorOf 5 (arbitraryOf l)
-- reify (ASelect l)   = pure [ Pick l ]

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
  | Just v <- scan t :: Maybe Nat = okay v
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper id"]

parseLabel :: Text -> Either Text Label
parseLabel t
  | Just (c, _) <- Text.uncons t, Char.isUpper c = okay <| t
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper label"]

parseConcrete :: Text -> Either Text Concrete
parseConcrete val
  | Just v <- scan val :: Maybe Unit = okay <| Concrete v
  | Just v <- scan val :: Maybe Bool = okay <| Concrete v
  | Just v <- scan val :: Maybe Int = okay <| Concrete v
  | Just v <- scan val :: Maybe (Int, Int) = okay <| Concrete v
  | Just v <- scan val :: Maybe Double = okay <| Concrete v
  | Just v <- scan val :: Maybe Text = okay <| Concrete v
  | Just v <- scan val :: Maybe [Bool] = okay <| Concrete v
  | Just v <- scan val :: Maybe [Int] = okay <| Concrete v
  | Just v <- scan val :: Maybe [Double] = okay <| Concrete v
  | Just v <- scan val :: Maybe [Text] = okay <| Concrete v
  | otherwise = error <| unwords ["!! Error parsing value", display val |> quote]

parse :: Text -> Either Text (Input Concrete)
parse t = case Text.words t of
  ["help"] -> error usage
  ["h"] -> error usage
  [i, x] -> do
    n <- parseId i
    map (Pick n) (parseLabel x) ++ map (Insert n) (parseConcrete x) --NOTE: should be `<|>`, but we've got some strange import of `Error` getting in the way
  [x] -> do
    l <- parseLabel x
    okay <| Prepick l
  _ -> error <| unwords ["!!", display t |> quote, "is not a valid command, type `help` for more info"]
