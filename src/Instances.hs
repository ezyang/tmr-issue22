{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Instances (

) where

import Classes
import Control.Monad              (liftM, ap)
import Control.Applicative        (Applicative(..))
import Control.Monad.Error        (ErrorT(..), MonadError(..))
import Control.Monad.State        (StateT(..))
import Control.Monad.Trans.Maybe  (MaybeT(..))


instance Plus Maybe where
  zero = Nothing
  Nothing <+> y = y
  x       <+> _ = x

instance (Functor m, Monad m) => Plus (MaybeT m) where
  zero = MaybeT (return Nothing)
  MaybeT l  <+>  MaybeT r  =  MaybeT x
    where
      x = l >>= \y -> case y of Nothing -> r;
                                Just _  -> return y;

instance (Monad m, Plus m) => Plus (ErrorT e m) where
  zero = ErrorT zero
  ErrorT a <+> ErrorT b = ErrorT (a <+> b)

instance (Monad m, Plus m) => Plus (StateT s m) where
  zero = StateT (const zero)
  StateT f <+> StateT g = StateT (\s -> f s <+> g s)


instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()
  
instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

instance Functor m => Switch (MaybeT m) where
  switch (MaybeT m) = MaybeT (fmap switch m)

  
instance Functor (Either' e) where
  fmap = liftM
    
instance Applicative (Either' e) where
  pure = return
  (<*>) = ap
  
instance Monad (Either' e) where
  return = Right'
  Left' e   >>=  _  =  Left' e
  Right' x  >>=  f  =  f x

instance MonadError e (Either' e) where
  throwError = Left'
  catchError (Right' x) _ = Right' x
  catchError (Left' e)  f = f e
