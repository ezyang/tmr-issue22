{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Parser (
    
    item
  , check
  , satisfy
  , Switch(..)
  , not1
  , optionalM
  , end
  , commit
  
) where

import Control.Monad.State  (MonadState(..), StateT(..))
import Control.Monad.Error  (MonadError(..))
import Control.Applicative  (Applicative(..))
import Classes


item :: (MonadState [t] m, Plus m) => m t
item =
    get >>= \xs -> case xs of
                        (t:ts) -> put ts *> pure t;
                        []     -> zero;

check :: (Monad m, Plus m) => (a -> Bool) -> m a -> m a
check f p =
    p >>= \x ->
    if (f x) then return x else zero

satisfy :: (MonadState [t] m, Plus m) => (t -> Bool) -> m t
satisfy = flip check item

class Switch f where
  switch :: f a -> f ()
  
instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()
  
instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

not1 :: (MonadState [t] m, Plus m, Switch m) => m a -> m t
not1 p = switch p *> item

optionalM :: (Functor f, Plus f) => f a -> f (Maybe a)
optionalM p = fmap Just p <+> pure Nothing

end :: (MonadState [t] m, Plus m, Switch m) => m ()
end = switch item

commit :: (MonadError e m, Plus m) => e -> m a -> m a
commit err p = p <+> throwError err
