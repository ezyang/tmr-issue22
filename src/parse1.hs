{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..))
import Control.Applicative       (Applicative(..))
import Data.List                 (nub)


-- the prelude:  type classes, data types, instances, and general combinators

class Applicative f => Plus f where
  zero :: f a
  (<+>) :: f a -> f a -> f a

many0 :: Plus f => f a -> f [a]
many0 p = many1 p <+> pure []

many1 :: Plus f => f a -> f [a]
many1 p = pure (:) <*> p <*> many0 p

instance Plus Maybe where
  zero = Nothing
  Nothing <+> y = y
  x       <+> _ = x

instance (Monad m, Plus m) => Plus (StateT s m) where
  zero = StateT (const zero)
  StateT f <+> StateT g = StateT (\s -> f s <+> g s)


class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

data AST
    = ANumber Integer
    | ASymbol String
    | AString String
    | ALambda [String] [AST]
    | ADefine String AST
    | AApp    AST  [AST]
  deriving (Show, Eq)

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

literal :: (Eq t, MonadState [t] m, Plus m) => t -> m t
literal c = satisfy ((==) c)

not1 :: (MonadState [t] m, Plus m, Switch m) => m a -> m t
not1 p = switch p *> item

end :: (MonadState [t] m, Plus m, Switch m) => m ()
end = switch item

example = "{define \n\
\  f \n\
\  {lambda {x y}\n\
\    (plus x y)}}\n\
\\n\
\(a b (c d e))\n\
\\n\
\; here's a nice comment !!\n\
\\n\
\"

-- the actual parser

whitespace = many1 $ satisfy (flip elem " \n\t\r\f")
comment = pure (:) <*> literal ';' <*> many0 (not1 $ literal '\n')

munch p = many0 (whitespace <+> comment) *> p

ocurly = munch $ literal '{'
ccurly = munch $ literal '}'
oparen = munch $ literal '('
cparen = munch $ literal ')'
symbol = munch $ many1 char
  where char = satisfy (flip elem (['a' .. 'z'] ++ ['A' .. 'Z']))

application =
    oparen      >>
    form        >>= \op ->
    many0 form  >>= \args ->
    cparen      >>
    return (AApp op args)

define =
    ocurly                       *>
    check (== "define") symbol   *>
    pure ADefine                <*>
    symbol                      <*>
    form                        <*
    ccurly
    
lambda =
    ocurly                          >>
    check (== "lambda") symbol      >>
    ocurly                          >>
    check distinct (many0 symbol)   >>= \params ->
    ccurly                          >>
    many1 form                      >>= \bodies ->
    ccurly                          >>
    return (ALambda params bodies)
  where
    distinct names = length names == length (nub names)

special = define <+> lambda

form = fmap ASymbol symbol <+> application <+> special

woof = many1 form <* munch end


type Parser a = StateT [Char] Maybe a 

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT
