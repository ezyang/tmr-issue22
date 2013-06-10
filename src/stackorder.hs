{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative       (Applicative(..), Alternative(..))
import Data.Functor.Identity     (Identity(..))


type Parser' t a = MaybeT (StateT [t] Identity) a

runParser' :: Parser' t a -> [t] -> (Maybe a, [t])
runParser' p xs = runIdentity $ runStateT (runMaybeT p) xs


item :: (MonadState [t] m, Alternative m) => m t
item =
    get >>= \xs -> case xs of
                        (t:ts) -> put ts *> pure t;
                        []     -> empty;

check :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
check f p =
    p >>= \x ->
    if (f x) then return x else empty

satisfy :: (MonadState [t] m, Alternative m) => (t -> Bool) -> m t
satisfy = flip check item

literal :: (Eq t, MonadState [t] m, Alternative m) => t -> m t
literal c = satisfy ((==) c)


type Stack1 e a = Either e (Maybe a)
type Stack2 e a = Maybe (Either e a)

forward :: Stack1 e a -> Stack2 e a
forward (Left e)          =  Just $ Left e
forward (Right Nothing)   =  Nothing
forward (Right (Just x))  =  Just $ Right x

backward :: Stack2 e a -> Stack1 e a
backward Nothing           =  Right Nothing
backward (Just (Left e))   =  Left e
backward (Just (Right x))  =  Right $ Just x

identityForward :: Stack1 e a -> Stack1 e a
identityForward = backward . forward

identityBackward :: Stack2 e a -> Stack2 e a
identityBackward = forward . backward