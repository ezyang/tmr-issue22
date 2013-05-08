{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..))
import Control.Applicative       (Applicative(..))
import Data.List                 (nub)
import Parser
import Classes
import Common
import Instances


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
