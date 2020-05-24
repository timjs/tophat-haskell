{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Prelude
  ( module Relude,
    -- module Control.Newtype,
    module Data.Type.Equality,

    -- * Types
    Unit,
    Nat,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    List,
    Cons,
    NonEmpty (..),
    -- Vector,

    -- * Classes
    Hash,
    Group (..),
    Module (..),
    Torsor (..),

    -- * Aliases
    length,
    (~>),
    getTextLn,

    -- ** Errors
    ok,
    error,

    -- ** Text
    chars,
    unchars,

    -- ** Reading
    Scan,
    scan,

    -- ** Tracing
    spy,

    -- ** HashMaps
    forWithKey,

    -- ** HashSets
    (=<),
    (/<),

    -- ** Vectors
    -- Vector, only, index, update

    -- * Pretty printing
    Pretty (..),
    Doc,
    viaShow,
    sep,
    cat,
    split,

    -- * Operators

    -- ** Pairs
    -- pattern (:=),

    -- ** Foldables
    foldr1,

    -- ** Monoids
    neutral,
    (++),

    -- ** Functions
    (|>),
    (<|),
    (.),
    (<<),
    (>>),

    -- ** Functors
    (<||),
    (||>),
    -- (<|.),
    -- (.|>),
    map,

    -- ** Applicatives
    (-<),
    (>-),
    (-|),
    (|-),
    lift0,
    lift1,
    lift2,
    lift3,
    (<-<),
    (>->),

    -- ** Monoidals
    Monoidal ((><), skip, (>|), (|<)),
    applyDefault,
    pureDefault,

    -- ** Selectives
    -- Selective (branch, select, biselect),
    -- check,
    -- when,

    -- * Fixes

    -- ** MonadZero
    MonadZero,
    fail,

    -- * Type level
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

-- import Control.Newtype hiding (pack, unpack)

import Data.Foldable (foldr1)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Doc, Pretty (..), viaShow)
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Type.Equality
import Data.Unique (Unique, hashUnique)
import Relude hiding
  ( ($),
    ($>),
    (&),
    (*>),
    (++),
    (++),
    (.),
    (<$),
    (<$>),
    (<&>),
    (<*),
    -- (<>),
    -- (<*>),
    (>>),
    Any,
    MonadFail (..),
    Nat,
    Option (..),
    Read,
    String,
    Word,
    Word16,
    Word32,
    Word64,
    Word8,
    error,
    first,
    forever,
    getLine,
    id,
    length,
    liftA2,
    liftA3,
    map,
    mempty,
    pass,
    readMaybe,
    second,
    trace,
    traceShow,
    traceShowId,
    when,
  )
import qualified Relude
import Type.Reflection (SomeTypeRep (..), TypeRep, someTypeRep, typeOf, typeRep)

-- import qualified Data.Vector as Vector

-- Synonyms --------------------------------------------------------------------

type Unit = ()

type Nat = Relude.Word

type Nat8 = Relude.Word8

type Nat16 = Relude.Word16

type Nat32 = Relude.Word32

type Nat64 = Relude.Word64

type List = []

type Cons = NonEmpty

type Hash a = (Eq a, Hashable a)

chars :: Text -> List Char
chars = Text.unpack

unchars :: List Char -> Text
unchars = Text.pack

length :: Foldable f => f a -> Nat
length = Relude.length >> fromIntegral

infix 0 ~>

(~>) :: a -> b -> (a, b)
(~>) = (,)

getTextLn :: MonadIO m => m Text
getTextLn = Relude.getLine

-- Pretty printing --

instance (Pretty v) => Pretty (HashSet v) where
  pretty = Pretty.braces << Pretty.cat << Pretty.punctuate ", " << map pretty << HashSet.toList

instance (Pretty k, Pretty v) => Pretty (HashMap k v) where
  pretty = Pretty.braces << Pretty.cat << Pretty.punctuate ", " << map (\(k, v) -> cat [pretty k, ": ", pretty v]) << HashMap.toList

instance Pretty Unique where
  pretty = pretty << hashUnique

-- HashMaps --

forWithKey :: Applicative f => HashMap k v -> (k -> v -> f w) -> f (HashMap k w)
forWithKey = flip HashMap.traverseWithKey

-- HashSets --

infix 4 =<

infix 4 /<

(=<) :: Hash a => a -> HashSet a -> Bool
(=<) = HashSet.member

(/<) :: Hash a => a -> HashSet a -> Bool
(/<) x = not << HashSet.member x

-- Vectors ---------------------------------------------------------------------

-- only :: a -> Vector a
-- only = Vector.singleton

-- index :: Nat -> Vector a -> Maybe a
-- index (Nat i) xs = (Vector.!?) xs i

-- update :: Nat -> a -> Vector a -> Vector a
-- update (Nat i) x xs = (Vector.//) xs [ ( i, x ) ]

-- instance ( Pretty a ) => Pretty (Vector a) where
--   pretty = Pretty.angles << fold << intersperse ", " << map pretty << Vector.toList

-- Reading & Tracing -----------------------------------------------------------

type Scan = Relude.Read

scan :: Scan a => Text -> Maybe a
scan = Relude.readMaybe << chars
{-# INLINE scan #-}

spy :: Pretty a => Text -> a -> a
spy m x = Relude.traceShow (pretty m ++ ": " ++ pretty x) x
{-# INLINE spy #-}

sep :: List (Doc n) -> Doc n
sep = Pretty.hsep

cat :: List (Doc n) -> Doc n
cat = Pretty.hcat

split :: List (Doc n) -> Doc n
split = Pretty.vsep

-- pretty' :: Pretty a => a -> Pretty.SimpleDocStream n
-- pretty' = Pretty.layoutPretty (Pretty.LayoutOptions Pretty.Unbounded) << pretty

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (x1, x2, x3, x4) = Pretty.tupled [pretty x1, pretty x2, pretty x3, pretty x4]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
  pretty (x1, x2, x3, x4, x5) = Pretty.tupled [pretty x1, pretty x2, pretty x3, pretty x4, pretty x5]

-- Monoids ---------------------------------------------------------------------

infixr 5 ++

(++) :: Relude.Semigroup s => s -> s -> s
(++) = (Relude.<>)
{-# INLINE (++) #-}

neutral :: Monoid m => m
neutral = Relude.mempty
{-# INLINE neutral #-}

-- Groups and more -------------------------------------------------------------

infixr 5 ~~

class Monoid a => Group a where
  invert :: a -> a
  invert x = neutral ~~ x

  (~~) :: a -> a -> a
  (~~) x y = x ++ invert y

class (Group a, Num s) => Module a s | a -> s where
  scale :: s -> a -> a

class Group d => Torsor a d | a -> d where
  diff :: a -> a -> d
  adjust :: d -> a -> a

-- Functions -------------------------------------------------------------------

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
(.) = flip (<|)
{-# INLINE (.) #-}

(<<) :: (b -> c) -> (a -> b) -> a -> c
f << g = \x -> f (g x)
{-# INLINE (<<) #-}

(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) = flip (<<)
{-# INLINE (>>) #-}

-- Functors --------------------------------------------------------------------

infixl 4 <||

infixl 1 ||>

map :: Functor f => (a -> b) -> f a -> f b
map = Relude.fmap
{-# INLINE map #-}

(<||) :: Functor f => (a -> b) -> f a -> f b
(<||) = map
{-# INLINE (<||) #-}

(||>) :: Functor f => f a -> (a -> b) -> f b
(||>) = flip (Relude.<$>)
{-# INLINE (||>) #-}

-- infixl 4 <|.

-- infixl 4 .|>

-- (<|.) :: Functor f => a -> f b -> f a
-- (<|.) = (Relude.<$)
-- {-# INLINE (<|.) #-}

-- (.|>) :: Functor f => f a -> b -> f b
-- (.|>) = (Relude.$>)
-- {-# INLINE (.|>) #-}

-- Applicative functors --------------------------------------------------------

infixr 1 <-<

infixr 1 >->

infixl 4 >-

infixl 4 -<

infixl 4 |-

infixl 4 -|

(-<) :: Applicative f => f (a -> b) -> f a -> f b
(-<) = (Relude.<*>)
{-# INLINE (-<) #-}

(>-) :: Applicative f => f a -> f (a -> b) -> f b
(>-) = flip (-<)
{-# INLINE (>-) #-}

(-|) :: Applicative f => f a -> f b -> f a
(-|) = (Relude.<*)
{-# INLINE (-|) #-}

(|-) :: Applicative f => f a -> f b -> f b
(|-) = (Relude.*>)
{-# INLINE (|-) #-}

lift0 :: Applicative f => a -> f a
lift0 = pure
{-# INLINE lift0 #-}

lift1 :: Functor f => (a -> b) -> f a -> f b
lift1 = map
{-# INLINE lift1 #-}

lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 = Relude.liftA2
{-# INLINE lift2 #-}

lift3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 = Relude.liftA3
{-# INLINE lift3 #-}

(<-<) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
f <-< g = pure (<<) -< f -< g
{-# INLINE (<-<) #-}

(>->) :: Applicative f => f (a -> b) -> f (b -> c) -> f (a -> c)
(>->) = flip (<-<)
{-# INLINE (>->) #-}

-- Monoidal functors -----------------------------------------------------------

infixl 6 ><

infixl 6 >|

infixl 6 |<

class Applicative f => Monoidal f where
  (><) :: f a -> f b -> f (a, b)
  (><) x y = pure (,) -< x -< y

  skip :: f ()
  skip = pure ()

  (>|) :: f a -> f b -> f a
  (>|) x y = pure fst -< x >< y

  (|<) :: f a -> f b -> f b
  (|<) x y = pure snd -< x >< y

applyDefault :: Monoidal f => f (a -> b) -> f a -> f b
applyDefault fg fx = pure (\(g, x) -> g x) -< fg >< fx

pureDefault :: Monoidal f => a -> f a
pureDefault x = map (const x) skip

instance Monoidal Maybe

instance Monoidal (Either e)

instance Monoidal IO

{- Selective functors ----------------------------------------------------------

class Applicative f => Selective f where
  branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
  branch p x y = map (map Left) p `select` map (map Right) x `select` y

  select :: f (Either a b) -> f (a -> b) -> f b
  select x y = branch x y (pure identity)

  biselect :: f (Either a b) -> f (Either a c) -> f (Either a (b, c))
  biselect x y = select (map Left << swp <|| x) ((\e a -> map (a,) e) <|| y)
    where
      swp = either Right Left

check :: Selective f => f Bool -> f a -> f a -> f a
check p t e = branch (map go p) (map const t) (map const e)
  where
    go x = if x then Right () else Left ()

when :: Selective f => f Bool -> f Unit -> f Unit
when p t = check p t (pure ())
-}

-- Monads ----------------------------------------------------------------------

-- Zero --

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

fail :: Alternative m => m a
fail = empty

-- instance Monad m => MonadZero (ListT m)

instance (Relude.MonadFail m, MonadPlus m) => MonadZero (StateT s m)

-- Error --

-- type Result = Either

-- pattern Ok :: a -> Result e a
-- pattern Ok x = Right x

-- pattern Err :: e -> Result e a
-- pattern Err e = Left e

ok :: a -> Either e a
ok = Right
{-# INLINE ok #-}

error :: e -> Either e a
error = Left
{-# INLINE error #-}

-- instance Alternative (Either e) where
--   Left e <|> y = Left e
--   x <|> _ = x

--   empty = Left neutral

-- Type equality ---------------------------------------------------------------

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

typeOfProxy :: forall a. Typeable a => Proxy a -> TypeRep a
typeOfProxy _ = typeRep

someTypeOf :: Typeable a => a -> SomeTypeRep
someTypeOf = someTypeRep << proxyOf
{-# INLINE someTypeOf #-}

instance Pretty (TypeRep a) where
  pretty = Pretty.viaShow

instance Pretty SomeTypeRep where
  pretty = Pretty.viaShow
