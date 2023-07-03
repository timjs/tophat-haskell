module Task.Input
  ( Concrete (..),
    Abstract (..),
    Input (..),
    usage,
    parse,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Task.Syntax (Basic, Id, Label)

---- Inputs --------------------------------------------------------------------

---- Concrete inputs

data Concrete :: Type where
  Concrete :: (Basic b) => b -> Concrete

instance Eq Concrete where
  Concrete x == Concrete y
    | Just Refl <- x ~= y = x == y
    | otherwise = False

instance Display Concrete where
  display = \case
    Concrete x -> display x

---- Abstract inputs

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

---- Inputs

data Input b
  = Insert Id b
  | Decide Id Label
  deriving (Eq, Debug, Functor, Foldable, Traversable)

instance (Display b) => Display (Input b) where
  display = \case
    Insert n b -> unwords [display n, display b]
    Decide n l -> unwords [display n, display l]

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

parseId :: Text -> Either Text Id
parseId t
  | Just v <- scan t :: Maybe Id = done v
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper id"]

parseLabel :: Text -> Either Text Label
parseLabel t
  | Just (c, _) <- Text.uncons t, Char.isUpper c = done <| t
  | otherwise = error <| unwords ["!!", display t |> quote, "is not a proper label"]

parseConcrete :: Text -> Either Text Concrete
parseConcrete val
  | Just v <- scan val :: Maybe Unit = done <| Concrete v
  | Just v <- scan val :: Maybe Bool = done <| Concrete v
  | Just v <- scan val :: Maybe Int = done <| Concrete v
  | Just v <- scan val :: Maybe (Int, Int) = done <| Concrete v
  | Just v <- scan val :: Maybe Double = done <| Concrete v
  | Just v <- scan val :: Maybe Text = done <| Concrete v
  | Just v <- scan val :: Maybe [Bool] = done <| Concrete v
  | Just v <- scan val :: Maybe [Int] = done <| Concrete v
  | Just v <- scan val :: Maybe [Double] = done <| Concrete v
  | Just v <- scan val :: Maybe [Text] = done <| Concrete v
  | otherwise = error <| unwords ["!! Error parsing value", display val |> quote]

parse :: Text -> Either Text (Input Concrete)
parse t
  | ("help", _) <- b = error usage
  | ("h", _) <- b = error usage
  | (i, r) <- b = do
      let x = Text.strip r
      k <- parseId i
      map (Decide k) (parseLabel x) ++ map (Insert k) (parseConcrete x) -- NOTE: should be `<|>`, but we've got some strange import of `Error` getting in the way
      -- _ -> error <| unwords ["!!", display t |> quote, "is not a valid command, type `help` for more info"]
  where
    b = Text.breakOn " " t
