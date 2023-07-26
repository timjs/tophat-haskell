module Task.Input
  ( Concrete (..),
    Abstract (..),
    Input (..),
    Action (..),
    usage,
    parse,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Task.Syntax (Basic, Id, Ix, Label)

---- Inputs --------------------------------------------------------------------

data Input b
  = Send Id (Action b)
  deriving (Eq, Debug, Functor, Foldable, Traversable)

data Action b
  = Insert b
  | Decide Label
  | -- | Fork Ix
    Kill Ix
  | Init
  deriving (Eq, Debug, Functor, Foldable, Traversable)

instance (Display b) => Display (Input b) where
  display = \case
    Send k a -> unwords [display k, display a]

instance (Display b) => Display (Action b) where
  display = \case
    Insert b -> display b
    Decide l -> "/" ++ display l
    -- Fork i -> "+" ++ display i
    Kill i -> "-" ++ display i
    Init -> "*"

---- Concrete ----

data Concrete :: Type where
  Concrete :: (Basic b) => b -> Concrete

instance Eq Concrete where
  Concrete x == Concrete y
    | Just Refl <- x ~= y = x == y
    | otherwise = False

instance Display Concrete where
  display = \case
    Concrete x -> display x

---- Abstract ----

data Abstract :: Type where
  Abstract :: (Basic b) => Proxy b -> Abstract

instance Eq Abstract where
  Abstract x == Abstract y
    -- NOTE: We're comparing proxies, they are always equal when the types are equal.
    | Just Refl <- x ~= y = True
    | otherwise = False

instance Display Abstract where
  display = \case
    Abstract p -> concat ["<", display beta, ">"]
      where
        beta = typeOfProxy p

---- Conformance ---------------------------------------------------------------

-- dummyfy :: Action -> Dummy
-- dummyfy = \case
--   Insert x -> AEnter (proxyOf x)
--   Pick l -> ASelect l

-- reify :: Dummy -> Gen (List Action)
-- reify (AEnter l)  = map Insert <$> vectorOf 5 (arbitraryOf l)
-- reify (ASelect l)   = done [ Pick l ]

-- strip :: Input Action -> Input Dummy
-- strip = map dummyfy

-- fill :: Input Dummy -> Gen (List (Input Action))
-- fill = map sequence << sequence << map reify

---- Parsing -------------------------------------------------------------------

usage :: Text
usage =
  unlines
    [ ":: Possible inputs are:",
      "    <id> <action> : send <action> to editor <id>",
      "    help, h       : show this message",
      "    quit, q       : quit",
      "",
      "where <action> is one of:",
      "    <value>       : input <value>",
      "    <label>       : select one of the possible options",
      "    +<ix>         : duplicate task at position <ix>",
      "    -<ix>         : terminate task at position <ix>",
      "    *             : start new task in pool",
      "",
      "where <id>s and <ix>s have the form:",
      "    0, 2, 37, …",
      "",
      "and <value>s can be:",
      "    ()           : Unit",
      "    True, False  : Booleans",
      "    0, 1, …      : Naturals",
      "    +1, -42, …   : Integers",
      "    \"Hello\", …   : Strings",
      "    [ <value>, ] : List of values",
      "",
      "and <label>s:",
      "    Start With A Capital Letter"
    ]

parseNat :: Text -> Either Text Nat
parseNat t
  | Just v <- scan t :: Maybe Nat = done v
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper id or ix"]

parseLabel :: Text -> Either Text Label
parseLabel t
  | Just (c, _) <- Text.uncons t, Char.isUpper c = done <| t
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper label"]

parseAction :: Text -> Either Text (Action Concrete)
parseAction t
  | Just (c, r) <- Text.uncons t = case c of
      -- '+' -> map Fork (parseNat r)
      '-' -> map Kill (parseNat r)
      '*' -> done Init
      _ -> map Decide (parseLabel r) ++ map Insert (parseConcrete t) -- NOTE: could be `<|>`, but has no general `empty` instance
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper action"]

parseConcrete :: Text -> Either Text Concrete
parseConcrete t
  | Just v <- scan t :: Maybe Unit = done <| Concrete v
  | Just v <- scan t :: Maybe Bool = done <| Concrete v
  | Just (s, r) <- Text.uncons t, s == '+' || s == '-', Just v <- scan r :: Maybe Int = done <| Concrete v
  | Just v <- scan t :: Maybe Nat = done <| Concrete v
  | Just v <- scan t :: Maybe Char = done <| Concrete v
  | Just v <- scan t :: Maybe Double = done <| Concrete v
  | Just v <- scan t :: Maybe Text = done <| Concrete v
  | Just v <- scan t :: Maybe [Bool] = done <| Concrete v
  | Just v <- scan t :: Maybe [Nat] = done <| Concrete v
  | Just v <- scan t :: Maybe [Char] = done <| Concrete v
  | Just v <- scan t :: Maybe [Double] = done <| Concrete v
  | Just v <- scan t :: Maybe [Text] = done <| Concrete v
  | Just v <- scan t :: Maybe [(Text, Nat)] = done <| Concrete v
  | Just v <- scan t :: Maybe [(Nat, Char)] = done <| Concrete v
  | Just v <- scan t :: Maybe (Bool, Bool) = done <| Concrete v
  | Just v <- scan t :: Maybe (Nat, Nat) = done <| Concrete v
  | Just v <- scan t :: Maybe (Char, Char) = done <| Concrete v
  | Just v <- scan t :: Maybe (Double, Double) = done <| Concrete v
  | Just v <- scan t :: Maybe (Text, Text) = done <| Concrete v
  | otherwise = error <| unwords ["!! Error parsing value", display t |> quote]

parse :: Text -> Either Text (Input Concrete)
parse t
  | ("help", _) <- b = error usage
  | ("h", _) <- b = error usage
  | (i, r) <- b = do
      k <- parseNat i
      a <- parseAction (Text.strip r)
      done <| Send k a
  where
    b = Text.breakOn " " t
