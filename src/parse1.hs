{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..))
import Control.Applicative       (Applicative(..), Alternative(..))
import Data.List                 (nub)


-- the prelude:  type classes, data types, instances, and general combinators

type Parser t a = StateT [t] Maybe a 

runParser :: Parser t a -> [t] -> Maybe (a, [t])
runParser = runStateT


many0 = many
many1 = some


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


class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

not1 :: (MonadState [t] m, Alternative m, Switch m) => m a -> m t
not1 p = switch p *> item

end :: (MonadState [t] m, Alternative m, Switch m) => m ()
end = switch item


data AST
    = ASymbol String
    | ALambda [String] [AST]
    | ADefine String AST
    | AApp    AST  [AST]
  deriving (Show, Eq)


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

junk = many0 (whitespace <|> comment) 

tok p = p <* junk

opencurly  = tok $ literal '{'
closecurly = tok $ literal '}'
openparen  = tok $ literal '('
closeparen = tok $ literal ')'
symbol = tok $ many1 char
  where char = satisfy (flip elem (['a' .. 'z'] ++ ['A' .. 'Z']))

application =
    openparen    *>
    pure AApp   <*>
    form        <*>
    many0 form  <*
    closeparen

define =
    check (== "define") symbol   *>
    pure ADefine                <*>
    symbol                      <*>
    form                        <*
    closecurly

lambda = 
    check (== "lambda") symbol      *>
    opencurly                       *>
    pure ALambda                   <*>
    check distinct (many0 symbol)  <*>
    (closecurly                     *>
     many1 form                    <*
     closecurly)
  where
    distinct names = length names == length (nub names)

special = opencurly *> (define <|> lambda)

form = fmap ASymbol symbol <|> application <|> special

endCheck = switch item

woof = junk *> many0 form <* endCheck

test = runParser woof example == r
  where r = Just ([ADefine "f" (ALambda ["x","y"] [AApp (ASymbol "plus") [ASymbol "x",ASymbol "y"]]),
                   AApp (ASymbol "a") [ASymbol "b",AApp (ASymbol "c") [ASymbol "d",ASymbol "e"]]],
                  "")
