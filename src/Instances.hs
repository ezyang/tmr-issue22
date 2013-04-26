module Instances (

) where

import Classes
import Control.Monad.Error  (ErrorT(..))
import Control.Monad.State  (StateT(..))
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