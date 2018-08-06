module Task

import public Control.Monad.Ref

import public Task.Universe
import public Task.Event
import public Task.Semantics
import public Task.Internal

%default total
%access export

infixr 4 #
infixl 3 <*>, <&>
infixr 2 <|>, <?>
infixl 1 >>=, >>?


-- Interfaces ------------------------------------------------------------------

-- Monoidal --

pure : (typeOf a) -> Task m a
pure = Edit . Just

(<&>) : Show (typeOf a) => Show (typeOf b) => Task m a -> Task m b -> Task m (PAIR a b)
(<&>) = All

unit : Task m (BASIC UNIT)
unit = pure ()

-- (<*>) : Show (typeOf a) => Show (typeOf b) => Task m (FunTy a b) -> Task m a -> Task m b
-- (<*>) t1 t2 = (\f,x => f x) <$> t1 <&> t2


-- Alternative --

(<|>) : Show (typeOf a) => Task m a -> Task m a -> Task m a
(<|>) = Any

(<?>) : Show (typeOf a) => Task m a -> Task m a -> Task m a
(<?>) = One

fail : Task m a
fail = Fail


-- Monad --

(>>=) : Show (typeOf a) => Task m a -> (typeOf a -> Task m b) -> Task m b
(>>=) = Then

(>>?) : Show (typeOf a) => Task m a -> (typeOf a -> Task m b) -> Task m b
(>>?) = Next

-- infixl 1 >>*
-- (>>*) : Show (typeOf a) => Task m a -> List (typeOf a -> (Bool, Task m b)) -> Task m b
-- (>>*) t fs            = t >>- convert fs where
--   convert : List (Universe.typeOf a -> (Bool, Task m b)) -> Universe.typeOf a -> Task m b
--   convert [] x        = fail
--   convert (f :: fs) x =
--     let
--       ( guard, next ) = f x
--     in
--     (if guard then next else fail) <|> convert fs x


-- Functor --

(<$>) : Show (typeOf a) => (typeOf a -> typeOf b) -> Task m a -> Task m b
(<$>) f t = do
  x <- t
  pure $ f x


-- Labels --

||| Infix operator to label a task
(#) : Show (typeOf a) => Label -> Task m a -> Task m a
(#) = Label


-- Extras --

ask : (b : BasicTy) -> Task m (BASIC b)
ask _ = Edit Nothing

watch : MonadRef l m => l (typeOf a) -> Task m a
watch = Watch
