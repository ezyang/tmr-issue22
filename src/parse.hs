{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..), State)
import Control.Applicative       (Applicative(..))
import Data.List                 (nub)
import Parser
import Classes
import Common
import Instances


character :: (MonadState [Char] m, Plus m) => Char -> m Char
character c = satisfy ((==) c)

ocurly = character '{'
ccurly = character '}'
oparen = character '('
cparen = character ')'

digit = satisfy (flip elem ['0' .. '9'])

number = munch $ many1 digit

symbolOpens = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "!@#$%^&*_-+=:?<>/"


whitespace :: (MonadState [Char] m, Plus m, Switch m) => m [Char]
whitespace = many1 $ satisfy (flip elem " \n\t\r\f")

comment :: (MonadState [Char] m, Plus m, Switch m) => m [Char]
comment = character ';' *> many0 (not1 $ character '\n')

munch :: (MonadState [Char] m, Plus m, Switch m) => m a -> m a
munch p = many0 (whitespace <+> comment) *> p


symbol = munch (fmap (:) first <*> rest)
  where
    first = satisfy (flip elem symbolOpens)
    rest = many0 (first <+> digit)
    
string :: (MonadState [Char] m, Plus m, Switch m) => m String
string = munch (dq *> many0 char <* dq)
  where
    dq = character '"'
    char = escape <+> normal
    escape = character '\\' *> slash_or_dq
    normal = not1 slash_or_dq
    slash_or_dq = character '\\' <+> character '"'


top = munch oparen
tcp = munch cparen
toc = munch ocurly
tcc = munch ccurly

tsymbol :: (MonadState [Char] m, Plus m, Switch m) => m AST
tsymbol = fmap ASymbol symbol

tstring :: (MonadState [Char] m, Plus m, Switch m) => m AST
tstring = fmap AString string

tnumber :: (MonadState [Char] m, Plus m, Switch m) => m AST
tnumber = fmap (ANumber . read) number

application :: (MonadState [Char] m, Plus m, Switch m) => m AST
application =
    top        >>= \open ->
    form       >>= \op ->
    many0 form  >>= \args ->
    tcp        >>
    return (AApp op args)
    
lambda :: (MonadState [Char] m, Plus m, Switch m) => m AST
lambda =
    check (== "lambda") symbol    >>
    toc                           >>
    check distinct (many0 symbol)  >>= \params ->
    tcc                           >>
    many1 form                     >>= \bodies ->
    return (ALambda params bodies)
  where
    distinct names = length names == length (nub names)

define :: (MonadState [Char] m, Plus m, Switch m) => m AST
define =
    check (== "define") symbol   *>
    pure ADefine                <*>
    optionalM string            <*>
    symbol                      <*>
    form
    
special :: (MonadState [Char] m, Plus m, Switch m) => m AST
special = 
    toc  >>= \open ->
    (lambda <+> define) >>= \val ->
    tcc  >>
    return val
    
form :: (MonadState [Char] m, Plus m, Switch m) => m AST
form = foldr (<+>) zero [tsymbol, tnumber, tstring, application, special]

parser :: (MonadState [Char] m, Plus m, Switch m) => m [AST]
parser = many0 form <* munch end


type Parser a = StateT [Char] Maybe a 

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT
