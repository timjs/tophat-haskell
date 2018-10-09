module Task


import public Control.Monad.Ref
import        Control.Monad.Trace

import public Task.Semantics
import public Task.Internal


%default total
%access export


infixr 4 #
infixl 3 <*>, <&>
infixr 2 <|>, <?>
infixl 1 >>=, >>?



-- Running ---------------------------------------------------------------------


public export
Task : Ty -> Type
Task = TaskT IO


public export
Loc : (b : Ty) -> Type
Loc b = IORef (typeOf b)


covering
run : Task a -> Input -> IO (Task a)
run = drive



-- Interfaces ------------------------------------------------------------------

-- Functor --


(<$>) : (typeOf a -> typeOf b) -> TaskT m a -> TaskT m b
(<$>) f t =
  Then t (\x => Edit (Just (f x)))



-- Monoidal --


pure : (typeOf a) -> TaskT m a
pure = Edit . Just


(<&>) : TaskT m a -> TaskT m b -> TaskT m (PAIR a b)
(<&>) = And


unit : TaskT m (PRIM UNIT)
unit = pure ()


-- (<*>) : TaskT m (FUN a b) -> TaskT m a -> TaskT m b
-- (<*>) t1 t2 = (\f,x => f x) <$> t1 <&> t2



-- Alternative --


(<|>) : TaskT m a -> TaskT m a -> TaskT m a
(<|>) = Or


(<?>) : TaskT m a -> TaskT m a -> TaskT m a
(<?>) = Xor


fail : TaskT m a
fail = Fail



-- Monad --


(>>=) : TaskT m a -> (typeOf a -> TaskT m b) -> TaskT m b
(>>=) = Then


(>>?) : TaskT m a -> (typeOf a -> TaskT m b) -> TaskT m b
(>>?) = Next


-- infixl 1 >>*
-- (>>*) : TaskT m a -> List (typeOf a -> (Bool, TaskT m b)) -> TaskT m b
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


ref : (b : Ty) -> typeOf b -> Task (LOC b)
ref _ x = lift $ ref x


deref : (b : Ty) -> Loc b -> Task b
deref _ l = lift $ deref l


assign : (b : Ty) -> Loc b -> typeOf b -> Task (PRIM UNIT)
assign _ l x = lift $ assign l x


modify : (b : Ty) -> Loc b -> (typeOf b -> typeOf b) -> Task (PRIM UNIT)
modify _ l f = lift $ modify l f


-- Labels --


||| Infix operator to label a task
(#) : Label -> TaskT m a -> TaskT m a
(#) = Label



-- Extras --


ask : (b : Ty) -> TaskT m b
ask _ = Edit Nothing


watch : MonadRef l m => l (typeOf b) -> TaskT m b
watch = Store
