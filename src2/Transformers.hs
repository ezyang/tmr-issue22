{-# LANGUAGE FunctionalDependencies #-}
module Transformers (
  
    Trans   (..)
  , TMaybe  (..)
  , TState  (..)
  , TError  (..)
  , TWriter (..)
  
  , Id      (..)
  , MaybeT  (..)
  , StateT  (..)
  , ErrorT  (..)
  , WriterT (..)
  
) where


class Trans t m where
  lift :: Monad m => m a -> t m a


class Monad m => TMaybe m where
  mzero :: m a

class Monad m => TState s m | m -> s where
  get :: m s
  put :: s -> m ()

class Monad m => TError e m | m -> e where
  throwE :: e -> m a
  catchE :: m a -> (e -> m a) -> m a
  
class Monad m => TWriter w m | m -> w where
  tell :: w -> m ()


  
newtype Id a =
    Id {getId :: a}
  deriving (Show)

newtype MaybeT m a =
    MaybeT {getMaybeT :: m (Maybe a)}

newtype StateT s m a =
    StateT {getStateT :: s -> m (s, a)}

newtype ErrorT e m a =
    ErrorT {getErrorT :: m (Either e a)}
    
newtype WriterT w m a =
    WriterT {getWriterT :: m (w, a)}
  