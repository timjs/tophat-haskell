module Control.Monad.Error


import Control.Catchable
import Control.Monad.Trans
import Control.Monad.Ref


%default total
%access public export



-- Interface -------------------------------------------------------------------


interface ( Monad m, Catchable m e ) => MonadError e (m : Type -> Type) | m where {}



-- Transformer -----------------------------------------------------------------


record ErrorT (e : Type) (m : Type -> Type) (a : Type) where
  constructor ER
  runErrorT : m (Either e a)


Functor m => Functor (ErrorT e m) where
  map f (ER m) = ER $ map (map f) m


Applicative m => Applicative (ErrorT e m) where
  pure x = ER $ pure (Right x)

  (ER f) <*> (ER a) = ER $ (<*>) <$> f <*> a


Monad m => Monad (ErrorT e m) where
  (ER x) >>= k = ER $ x >>= either (pure . Left) (runErrorT . k)


Monad m => Catchable (ErrorT e m) e where
  throw = ER . pure . Left

  catch (ER m) h = ER $ do
    v <- m
    case v of
      Left  e => runErrorT (h e)
      Right x => pure $ Right x


MonadTrans (ErrorT e) where
  lift = ER . map Right



-- Instances -------------------------------------------------------------------


(Monad m, Catchable m e) => MonadError e m where {}

(Monad m, MonadRef l m) => MonadRef l (ErrorT e m) where
  ref x = lift $ ref x

  deref l = lift $ deref l

  assign l y = lift $ assign l y
