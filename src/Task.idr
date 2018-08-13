module Task


import public Control.Monad.Ref
import public Control.Monad.Error

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

-- Functor --


(<$>) : Show (typeOf a) => (typeOf a -> typeOf b) -> TaskT m a -> TaskT m b
(<$>) f t =
  Then t (\x => Edit (Just (f x)))



-- Monoidal --


pure : (typeOf a) -> TaskT m a
pure = Edit . Just


(<&>) : Show (typeOf a) => Show (typeOf b) => TaskT m a -> TaskT m b -> TaskT m (PAIR a b)
(<&>) = All


unit : TaskT m (BASIC UNIT)
unit = pure ()


-- (<*>) : Show (typeOf a) => Show (typeOf b) => TaskT m (FUN a b) -> TaskT m a -> TaskT m b
-- (<*>) t1 t2 = (\f,x => f x) <$> t1 <&> t2



-- Alternative --


(<|>) : Show (typeOf a) => TaskT m a -> TaskT m a -> TaskT m a
(<|>) = Any


(<?>) : Show (typeOf a) => TaskT m a -> TaskT m a -> TaskT m a
(<?>) = One


fail : TaskT m a
fail = Fail



-- Monad --


(>>=) : Show (typeOf a) => TaskT m a -> (typeOf a -> TaskT m b) -> TaskT m b
(>>=) = Then


(>>?) : Show (typeOf a) => TaskT m a -> (typeOf a -> TaskT m b) -> TaskT m b
(>>?) = Next


-- infixl 1 >>*
-- (>>*) : Show (typeOf a) => TaskT m a -> List (typeOf a -> (Bool, TaskT m b)) -> TaskT m b
-- (>>*) t fs            = t >>- convert fs where
--   convert : List (Universe.typeOf a -> (Bool, TaskT m b)) -> Universe.typeOf a -> TaskT m b
--   convert [] x        = fail
--   convert (f :: fs) x =
--     let
--       ( guard, next ) = f x
--     in
--     (if guard then next else fail) <|> convert fs x


-- Transformer --


lift : Monad m => m (typeOf a) -> TaskT m a
lift = Lift



-- Labels --


||| Infix operator to label a task
(#) : Show (typeOf a) => Label -> TaskT m a -> TaskT m a
(#) = Label



-- Extras --


ask : (b : BasicTy) -> TaskT m (BASIC b)
ask _ = Edit Nothing


watch : MonadRef l m => l (typeOf b) -> TaskT m (BASIC b)
watch = Watch
