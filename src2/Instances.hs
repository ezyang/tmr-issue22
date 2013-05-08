-- transformers:
--   functor, applicative, monad
--   switch, alternative

-- Id:  ???every type class???

-- Switch:  ???every datatype???

module Instances (
  
) where

import Transformers
import Control.Applicative  (Applicative(..), Alternative(..))
import Control.Monad        (ap, liftM)


class Switch m where
  switch :: m a -> m ()

instance Switch Maybe where
  switch  (Just _)  =  Nothing
  switch  Nothing   =  Just ()


instance Monad m => Functor (StateT s m) where
  fmap = liftM

instance Monad m => Applicative (StateT s m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (StateT s m) where
  return x = StateT (\s -> return (s, x))
  (StateT m1) >>= f = StateT h
    where
      h s1 = 
          m1 s1 >>= \(s2, x) ->
          getStateT (f x) s2

instance Alternative (StateT s m) where
  empty = undefined
  (<|>) = undefined
{-
-- Plus, Zero, Switch:  'lift' semantics
instance Plus m => Plus (StateT s m) where
  StateT f  <+>  StateT g  =  StateT (\s -> f s <+> g s)
  
instance Zero m => Zero (StateT s m) where
  zero = StateT (const zero)

instance Switch m => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const (s, ())) . switch $ f s)



instance Functor m => Functor (ErrorT e m) where
  fmap f (ErrorT m) = ErrorT (fmap (fmap f) m)

instance Pointed m => Pointed (ErrorT e m) where
  pure = ErrorT . pure . pure

instance Applicative' m => Applicative' (ErrorT e m) where
  ErrorT f <*> ErrorT x = ErrorT (pure (<*>) <*> f <*> x)

instance Monad' m => Monad' (ErrorT e m) where
  join =
      ErrorT                 .
      fmap join              .
      join                   .
      fmap commute           .
      fmap (fmap getErrorT)  .
      getErrorT

-- Plus, Zero, Switch:  'lift' semantics
instance Plus m => Plus (ErrorT e m) where
  ErrorT l  <+>  ErrorT r  =  ErrorT (l <+> r)
  
instance Zero m => Zero (ErrorT e m) where
  zero = ErrorT zero

instance Switch m => Switch (ErrorT e m) where
  switch (ErrorT e) =  ErrorT (fmap Right $ switch e)



instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT (fmap (fmap f) m)

instance Pointed m => Pointed (MaybeT m) where
  pure = MaybeT . pure . pure

instance Applicative' m => Applicative' (MaybeT m) where
  MaybeT f <*> MaybeT x = MaybeT (pure (<*>) <*> f <*> x)

instance Monad' m => Monad' (MaybeT m) where
  join =
      MaybeT                 .
      fmap join              .
      join                   .
      fmap commute           .
      fmap (fmap getMaybeT)  .
      getMaybeT

-- Plus, Zero, Switch:  deal with them here
instance Monad' m => Plus (MaybeT m) where
  MaybeT l  <+>  MaybeT r  =  MaybeT x
    where
      x = l >>== \y -> case y of Nothing -> r;
                                 Just _  -> pure y;

instance Monad' m => Zero (MaybeT m) where
  zero = MaybeT (pure Nothing)

instance Monad' m => Switch (MaybeT m) where
  switch (MaybeT x) = MaybeT (fmap switch x)
  
  

instance Functor m => Functor (WriterT w m) where
  fmap f (WriterT x) = WriterT (fmap (fmap f) x)

instance (Pointed m, Monoid' w) => Pointed (WriterT w m) where
  pure = WriterT . pure . pure
  
instance (Applicative' m, Monoid' w) => Applicative' (WriterT w m) where
  WriterT f <*> WriterT x = WriterT (pure (<*>) <*> f <*> x)
  
instance (Monad' m, Monoid' w) => Monad' (WriterT w m) where
  join = 
      WriterT                 .
      fmap join               .
      join                    .
      fmap commute            .
      fmap (fmap getWriterT)  .
      getWriterT

instance Plus m => Plus (WriterT w m) where
  WriterT l  <+>  WriterT r  =  WriterT (l <+> r)
  
instance Zero m => Zero (WriterT w m) where
  zero = WriterT zero
  
instance (Switch m, Monoid' w) => Switch (WriterT w m) where
  switch (WriterT x) = WriterT (fmap (const (empty, ())) $ switch x)
-}