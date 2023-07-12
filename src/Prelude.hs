{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Prelude
  ( -- * Reexports
    module Relude,
    module Data.Witherable,
    -- module Control.Newtype,
    module Data.Type.Equality,

    -- * Types
    Unit,
    Nat,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Relude.String,
    List,
    Assoc,
    Cons,
    NonEmpty (..),
    -- Vector,

    -- * Classes

    -- Hash,
    -- Typeable,
    -- Foldable,
    -- Traversable,
    -- Map,
    -- Apply,
    -- Choose,
    -- Monad,
    -- Coerce,

    -- ** Group, Module, Torsor
    Group (..),
    Module (..),
    Torsor (..),

    -- ** Scanning
    Scan,
    scan,

    -- ** Debugging
    Debug,
    debug,
    spy,

    -- ** Displaying
    Display (..),

    -- * Functions

    -- ** IO
    getTextLn,

    -- ** Tuples
    (~>),

    -- ** Text
    chars,
    unchars,
    between,
    quote,
    indent,

    -- ** Lists
    lookup,
    keys,

    -- ** HashMaps
    forWithKey,

    -- ** HashSets
    (=<),
    (/<),

    -- ** Vectors

    -- Vector,
    -- only,
    -- index,
    -- update,

    -- ** Maybes
    withDefault,

    -- ** Eithers
    error,
    -- note,
    -- hush,

    -- ** Folds
    length,
    same,
    intercalate,
    surroundMap,
    surround,
    foldr1,
    gather,
    -- gather1,

    -- ** Traversals
    for,

    -- ** Combines
    (++),
    neutral,
    concat,

    -- * Operators

    -- ** Functions
    (|>),
    (<|),
    (.),
    (<<),
    (>>),

    -- ** Functors
    (<-<),
    (>->),
    -- (<<-),
    -- (->>),
    map,
    Relude.fmap,

    -- ** Applicatives
    (-<<),
    (>>-),
    (-<-),
    (->-),
    (Relude.<*>),
    Relude.pure,
    done,
    lift0,
    lift1,
    lift2,
    lift3,
    -- (<-<),
    -- (>->),

    -- ** Alternatives
    fail,

    -- ** Monoidals
    Monoidal ((<&>), (&>), (<&), none),
    applyDefault,
    pureDefault,

    -- ** Selectives

    -- Selective (branch, select, biselect),
    -- check,
    -- when,

    -- ** Monads
    Relude.return,
    MonadZero,

    -- * Types and Proxys
    (~=),
    (~:),
    proxyOf,
    typeOf,
    typeOfProxy,
    someTypeOf,
    typeRep,
    TypeRep,
    someTypeRep,
    SomeTypeRep (..),
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (lookup)
import qualified Data.Text as Text
import Data.Type.Equality
import Data.Witherable
import Relude hiding
  ( Any,
    MonadFail (..),
    Nat,
    Read,
    Show,
    State,
    String,
    Word,
    Word16,
    Word32,
    Word64,
    Word8,
    catMaybes,
    concat,
    error,
    filter,
    first,
    fmap,
    foldlM,
    forever,
    fromMaybe,
    getLine,
    gets,
    hashNub,
    id,
    intercalate,
    length,
    liftA2,
    liftA3,
    map,
    mapMaybe,
    mconcat,
    mempty,
    ordNub,
    ordNubOn,
    pass,
    print,
    pure,
    readMaybe,
    return,
    second,
    show,
    trace,
    traceShow,
    traceShowId,
    when,
    ($),
    ($>),
    (&),
    (*>),
    (++),
    (.),
    (<$),
    (<$>),
    (<&>),
    (<*),
    (<*>),
    (<>),
    (=<<),
    (>>),
  )
import qualified Relude
import Type.Reflection (SomeTypeRep (..), TypeRep, someTypeRep, typeOf, typeRep)

-- import qualified Data.Vector as Vector
-- import Control.Newtype hiding (pack, unpack)

---- Types ---------------------------------------------------------------------

type Unit = ()

type Nat = Relude.Word

type Nat8 = Relude.Word8

type Nat16 = Relude.Word16

type Nat32 = Relude.Word32

type Nat64 = Relude.Word64

type List = []

type Assoc k v = List (k, v)

type Cons = NonEmpty

---- Classes -------------------------------------------------------------------

{-
type Calc = Relude.Num

type Hash a = (Eq a, Relude.Hashable a)

type Reflect = Relude.Typeable

type Coerce = Relude.Coercible

type Traverse = Relude.Traversable

type Map = Relude.Functor

type Apply = Relude.Applicative

type Choose = Relude.Alternative
-}
---- Group, Module, Torsor ----

infixr 5 ~~

class (Monoid a) => Group a where
  invert :: a -> a
  invert x = neutral ~~ x

  (~~) :: a -> a -> a
  (~~) x y = x ++ invert y

class (Group a, Num s) => Module a s | a -> s where
  scale :: s -> a -> a

class (Group d) => Torsor a d | a -> d where
  diff :: a -> a -> d
  adjust :: d -> a -> a

---- Scan ----

type Scan = Relude.Read

scan :: (Scan a) => Text -> Maybe a
scan = Relude.readMaybe << chars
{-# INLINE scan #-}

---- Debug ----

type Debug = Relude.Show

debug :: (Debug a) => a -> Text
debug = Relude.show
{-# INLINE debug #-}

spy :: (Debug a) => Text -> a -> a
spy m x = Relude.traceShow (debug m ++ ": " ++ debug x) x
{-# INLINE spy #-}

---- Displaying

-- | `Display` is like `Debug` but for user facing output.
class Display a where
  display :: a -> Text

instance Display () where
  display () = "()"

instance Display Bool where
  display = debug

instance Display Nat where
  display = debug

instance Display Int where
  display = debug

instance Display Double where
  display = debug

instance Display Char where
  display = Text.singleton

instance Display Text where
  display = identity

instance (Display e, Display a) => Display (Either e a) where
  display = \case
    Left e -> "Error: " ++ display e
    Right a -> display a

instance (Display a, Display b) => Display (a, b) where
  display (a, b) = display a ++ "," ++ display b |> between '(' ')'

instance (Display a) => Display (List a) where
  display = map display >> intercalate "," >> between '[' ']'

instance (Display k, Display v) => Display (HashMap k v) where
  display = HashMap.toList >> map (\(k, v) -> display k ++ ":" ++ display v) >> intercalate "," >> between '{' '}'

instance (Display v) => Display (HashSet v) where
  display = HashSet.toList >> map display >> intercalate "," >> between '{' '}'

instance Display (TypeRep a) where
  display = debug

instance Display SomeTypeRep where
  display = debug

---- Functions -----------------------------------------------------------------

same :: (Eq a) => List a -> Bool
same = \case
  [] -> True
  x : xs -> all (x ==) xs
{-# INLINE same #-}

---- IO ----

getTextLn :: (MonadIO m) => m Text
getTextLn = Relude.getLine
{-# INLINE getTextLn #-}

---- Tuples ----

infixr 0 ~>

(~>) :: a -> b -> (a, b)
(~>) = (,)
{-# INLINE (~>) #-}

---- Booleans

{-
infixr 3 &&&

infixr 2 |||

(&&&) :: forall a. (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) f g x = f x && g x

(|||) :: forall a. (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f g x = f x || g x
-}

---- Text ----

chars :: Text -> List Char
chars = Text.unpack
{-# INLINE chars #-}

unchars :: List Char -> Text
unchars = Text.pack
{-# INLINE unchars #-}

between :: Char -> Char -> Text -> Text
between a b t = a `Text.cons` t `Text.snoc` b
{-# INLINE between #-}

quote :: Text -> Text
quote = between '"' '"'
{-# INLINE quote #-}

indent :: Int -> Text -> Text
indent n t = Text.replicate n " " ++ t

---- Lists ----

keys :: Assoc k v -> List k
-- keys = HashMap.keys >> HashSet.fromList
keys = map fst

---- HashMaps ----

forWithKey :: (Applicative f) => HashMap k v -> (k -> v -> f w) -> f (HashMap k w)
forWithKey = flip HashMap.traverseWithKey
{-# INLINE forWithKey #-}

---- HashSets ----

infix 4 =<

infix 4 /<

(=<) :: (Hashable a) => a -> HashSet a -> Bool
(=<) = HashSet.member
{-# INLINE (=<) #-}

(/<) :: (Hashable a) => a -> HashSet a -> Bool
(/<) x = not << HashSet.member x
{-# INLINE (/<) #-}

---- Vectors ----

-- only :: a -> Vector a
-- only = Vector.singleton

-- index :: Nat -> Vector a -> Maybe a
-- index (Nat i) xs = (Vector.!?) xs i

-- update :: Nat -> a -> Vector a -> Vector a
-- update (Nat i) x xs = (Vector.//) xs [ ( i, x ) ]

---- Maybes ----

withDefault :: a -> Maybe a -> a
withDefault = Relude.fromMaybe

---- Eithers ----

error :: e -> Either e a
error = Left
{-# INLINE error #-}

-- note :: a -> Maybe b -> Either a b
-- note = maybeToRight
-- {-# INLINE note #-}

-- hush :: Either l r -> Maybe r
-- hush = rightToMaybe
-- {-# INLINE hush #-}

-- type Result = Either

-- pattern Ok :: a -> Result e a
-- pattern Ok x = Right x

-- pattern Err :: e -> Result e a
-- pattern Err e = Left e
-- instance Alternative (Either e) where
--   Left e <|> y = Left e
--   x <|> _ = x

--   empty = Left neutral

---- Folds ----

-- infix 4 /.
-- infix 4 .\

-- (/.) :: (Foldable t) => b -> t a -> (b -> a -> b) -> b
-- (/.) x xs f = foldl' f x xs

-- (.\) :: (Foldable t) => t a -> b -> (a -> b -> b) -> b
-- (.\) xs x f = foldr f x xs

length :: (Foldable t) => t a -> Nat
length = Relude.length >> fromIntegral
{-# INLINE length #-}

-- | Foldable a data structure, accumulating values in some `Monoid`,
-- | combining adjacent elements using the specified separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > intercalate ", " ["Lorem", "ipsum", "dolor"]
-- | = "Lorem, ipsum, dolor"
-- |
-- | > intercalate "*" ["a", "b", "c"]
-- | = "a*b*c"
-- |
-- | > intercalate [1] [[2, 3], [4, 5], [6, 7]]
-- | = [2, 3, 1, 4, 5, 1, 6, 7]
-- | ```
intercalate :: (Foldable f, Monoid m) => m -> f m -> m
intercalate sep = foldl' go (True, neutral) >> snd
  where
    go (True, _) x = (False, x)
    go (st, acc) x = (st, acc ++ sep ++ x)
{-# INLINE intercalate #-}

-- | `foldMap` but with each element surrounded by some fixed value.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > surroundMap "*" show []
-- | = "*"
-- |
-- | > surroundMap "*" show [1]
-- | = "*1*"
-- |
-- | > surroundMap "*" show [1, 2]
-- | = "*1*2*"
-- |
-- | > surroundMap "*" show [1, 2, 3]
-- | = "*1*2*3*"
-- | ```
surroundMap :: (Foldable f, Semigroup m) => m -> (a -> m) -> f a -> m
surroundMap d t f = appEndo (foldMap joined f) d
  where
    joined a = Endo \m -> d ++ t a ++ m
{-# INLINE surroundMap #-}

-- | `fold` but with each element surrounded by some fixed value.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > surround "*" []
-- | = "*"
-- |
-- | > surround "*" ["1"]
-- | = "*1*"
-- |
-- | > surround "*" ["1", "2"]
-- | = "*1*2*"
-- |
-- | > surround "*" ["1", "2", "3"]
-- | = "*1*2*3*"
-- | ```
surround :: forall f m. (Foldable f, Semigroup m) => m -> f m -> m
surround d = surroundMap d identity
{-# INLINE surround #-}

gather :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
gather = Relude.foldlM

-- gather f b0 = foldr go (done b0)
-- where
--   go a mb = mb >>= flip f a

-- gather1 :: (Monad m, Foldable t) => (a -> a -> m a) -> t a -> Maybe (m a)
-- gather1 :: (Monad m, Foldable t1) => (b -> t2 -> m b) -> b -> t1 t2 -> m b
-- gather1 :: (Foldable t, Monad m) => (a -> m a -> m a) -> t (m a) -> m (Maybe a)
-- gather1 :: (Foldable t, Monad m) => (a -> a -> m a) -> t a -> m (Maybe a)
-- gather1 f = foldr1 (\a ma -> _) >> sequence
-- gahter1 f = foldr go

foldr1 :: (Foldable t) => (a -> a -> a) -> t a -> Maybe a
foldr1 f = foldr mf Nothing
  where
    mf x m =
      Just <| case m of
        Nothing -> x
        Just y -> f x y

---- Traversals ----

for :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
for = Relude.forM

---- Combines ----

infixr 5 ++

(++) :: (Semigroup s) => s -> s -> s
(++) = (Relude.<>)
{-# INLINE (++) #-}

neutral :: (Monoid m) => m
neutral = Relude.mempty
{-# INLINE neutral #-}

concat :: (Monoid m) => List m -> m
concat = Relude.mconcat
{-# INLINE concat #-}

---- Operators -----------------------------------------------------------------

---- Functions ----

infixr 0 <|

infixl 1 |>

infixl 8 .

infixr 9 <<

infixr 9 >>

(<|) :: (a -> b) -> a -> b
(<|) f x = f x
{-# INLINE (<|) #-}

(|>) :: a -> (a -> b) -> b
(|>) = flip (<|)
{-# INLINE (|>) #-}

(.) :: a -> (a -> b) -> b
(.) = (|>)
{-# INLINE (.) #-}

(<<) :: (b -> c) -> (a -> b) -> a -> c
f << g = \x -> f (g x)
{-# INLINE (<<) #-}

(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) = flip (<<)
{-# INLINE (>>) #-}

---- Functors ----

infixl 4 <-<

infixl 1 >->

map :: (Functor f) => (a -> b) -> f a -> f b
map = Relude.fmap
{-# INLINE map #-}

(<-<) :: (Functor f) => (a -> b) -> f a -> f b
(<-<) = map
{-# INLINE (<-<) #-}

(>->) :: (Functor f) => f a -> (a -> b) -> f b
(>->) = flip (Relude.<$>)
{-# INLINE (>->) #-}

-- infixl 4 <<-
-- infixl 4 ->>

-- (<<-) :: (Functor f) => a -> f b -> f a
-- (<<-) = (Relude.<$)
-- {-# INLINE (<<-) #-}

-- (->>) :: (Functor f) => f a -> b -> f b
-- (->>) = (Relude.$>)
-- {-# INLINE (->>) #-}

---- Applicatives ----

infixl 4 >>-

infixl 4 -<<

infixl 4 ->-

infixl 4 -<-

(-<<) :: (Applicative f) => f (a -> b) -> f a -> f b
(-<<) = (Relude.<*>)
{-# INLINE (-<<) #-}

(>>-) :: (Applicative f) => f a -> f (a -> b) -> f b
(>>-) = flip (-<<)
{-# INLINE (>>-) #-}

(-<-) :: (Applicative f) => f a -> f b -> f a
(-<-) = (Relude.<*)
{-# INLINE (-<-) #-}

(->-) :: (Applicative f) => f a -> f b -> f b
(->-) = (Relude.*>)
{-# INLINE (->-) #-}

done :: (Applicative f) => a -> f a
done = Relude.pure

lift0 :: (Applicative f) => a -> f a
lift0 = Relude.pure
{-# INLINE lift0 #-}

lift1 :: (Functor f) => (a -> b) -> f a -> f b
lift1 = map
{-# INLINE lift1 #-}

lift2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
lift2 = Relude.liftA2
{-# INLINE lift2 #-}

lift3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 = Relude.liftA3
{-# INLINE lift3 #-}

-- (<-<) :: (Applicative f) => f (b -> c) -> f (a -> b) -> f (a -> c)
-- f <-< g = done (<<) -<< f -<< g
-- {-# INLINE (<-<) #-}

-- (>->) :: (Applicative f) => f (a -> b) -> f (b -> c) -> f (a -> c)
-- (>->) = flip (<-<)
-- {-# INLINE (>->) #-}

---- Alternatives ----

fail :: (Alternative m) => m a
fail = empty

---- Monoidals ----

infixl 6 <&>

infixl 6 <&

infixl 6 &>

class (Applicative f) => Monoidal f where
  (<&>) :: f a -> f b -> f (a, b)
  (<&>) x y = done (,) -<< x -<< y

  none :: f ()
  none = done ()

  (<&) :: f a -> f b -> f a
  (<&) x y = done fst -<< x <&> y

  (&>) :: f a -> f b -> f b
  (&>) x y = done snd -<< x <&> y

applyDefault :: (Monoidal f) => f (a -> b) -> f a -> f b
applyDefault fg fx = done (\(g, x) -> g x) -<< fg <&> fx

pureDefault :: (Monoidal f) => a -> f a
pureDefault x = map (const x) none

instance Monoidal Maybe

instance Monoidal (Either e)

instance Monoidal IO

----- Selectives ----
{-
class (Applicative f) => Selective f where
  branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
  branch p x y = map (map Left) p `select` map (map Right) x `select` y

  select :: f (Either a b) -> f (a -> b) -> f b
  select x y = branch x y (done identity)

  biselect :: f (Either a b) -> f (Either a c) -> f (Either a (b, c))
  biselect x y = select (map Left << swp <-< x) ((\e a -> map (a,) e) <-< y)
    where
      swp = either Right Left

check :: (Selective f) => f Bool -> f a -> f a -> f a
check p t e = branch (map go p) (map const t) (map const e)
  where
    go x = if x then Right () else Left ()

when :: (Selective f) => f Bool -> f Unit -> f Unit
when p t = check p t (done ())
-}

---- Monads ----

-- | A safe alternative for MonadFail.
-- |
-- | Be sure only types that have a sensible, non-throwing,
-- | `Alternative` instance are instances of `MonadZero`!
-- |
-- | Laws:
-- |   * fail == empty
class (Monad m, Relude.MonadFail m, Alternative m) => MonadZero m

instance MonadZero Maybe

instance MonadZero List

-- instance (Relude.MonadFail m, MonadPlus m) => MonadZero (StateT s m)

---- Type equality -------------------------------------------------------------

infix 4 ~=

infix 4 ~:

-- infix 4 ~?

(~=) :: (Typeable a, Typeable b) => a -> b -> Maybe (a :~: b)
(~=) x y = typeOf x `testEquality` typeOf y
{-# INLINE (~=) #-}

(~:) :: (Typeable a) => a -> TypeRep b -> Maybe (a :~: b)
(~:) x t = typeOf x `testEquality` t
{-# INLINE (~:) #-}

-- (~?) :: TestEquality f => f a -> f b -> Maybe (a :~: b)
-- (~?) = testEquality
-- {-# INLINE (~?) #-}

proxyOf :: a -> Proxy a
proxyOf _ = Proxy
{-# INLINE proxyOf #-}

typeOfProxy :: forall a. (Typeable a) => Proxy a -> TypeRep a
typeOfProxy _ = typeRep

someTypeOf :: (Typeable a) => a -> SomeTypeRep
someTypeOf = someTypeRep << proxyOf
{-# INLINE someTypeOf #-}
