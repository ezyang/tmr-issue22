{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..), State)
import Control.Applicative       (Applicative(..))
import Data.List                 (nub)
import Parser
import Classes
import Common


character :: (MonadState [Token] m, Plus m) => Char -> m Token
character c = satisfy ((==) c . chr)

ocurly = character '{'

ccurly = character '}'

oparen = character '('

cparen = character ')'

digit = fmap chr $ satisfy (flip elem ['0' .. '9'] . chr)

number = munch $ many1 digit

symbolOpens = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "!@#$%^&*_-+=:?<>/"

symbol = munch (fmap (:) first <*> rest)
  where
    first = fmap chr $ satisfy (flip elem symbolOpens . chr)
    rest = many0 (first <+> digit)
    
string :: (MonadState [Token] m, Plus m, Switch m) => m String
string = munch (dq *> many0 char <* dq)
  where
    dq = character '"'
    char = fmap chr (escape <+> normal)
    escape = character '\\' *> slash_or_dq
    normal = not1 slash_or_dq
    slash_or_dq = character '\\' <+> character '"'


whitespace :: (MonadState [Token] m, Plus m, Switch m) => m [Token]
whitespace = many1 $ satisfy (flip elem " \n\t\r\f" . chr)

comment :: (MonadState [Token] m, Plus m, Switch m) => m [Token]
comment = character ';' *> many0 (not1 $ character '\n')

munch :: (MonadState [Token] m, Plus m, Switch m) => m a -> m a
munch p = many0 (whitespace <+> comment) *> p

top = munch oparen
tcp = munch cparen
toc = munch ocurly
tcc = munch ccurly

tsymbol :: (MonadState [Token] m, Plus m, Switch m) => m AST
tsymbol = fmap ASymbol symbol

tstring :: (MonadState [Token] m, Plus m, Switch m) => m AST
tstring = fmap AString string

tnumber :: (MonadState [Token] m, Plus m, Switch m) => m AST
tnumber = fmap (ANumber . read) number

application :: (MonadState [Token] m, Plus m, Switch m) => m AST
application =
    top        >>= \open ->
    form       >>= \op ->
    many0 form  >>= \args ->
    tcp        >>
    return (AApp op args)
    
lambda :: (MonadState [Token] m, Plus m, Switch m) => m AST
lambda =
    check (== "lambda") symbol    >>
    toc                           >>
    check distinct (many0 symbol)  >>= \params ->
    tcc                           >>
    many1 form                     >>= \bodies ->
    return (ALambda params bodies)
  where
    distinct names = length names == length (nub names)

define :: (MonadState [Token] m, Plus m, Switch m) => m AST
define =
    check (== "define") symbol   *>
    pure ADefine                <*>
    optionalM string            <*>
    symbol                      <*>
    form
    
special :: (MonadState [Token] m, Plus m, Switch m) => m AST
special = 
    toc  >>= \open ->
    (lambda <+> define) >>= \val ->
    tcc  >>
    return val
    
form :: (MonadState [Token] m, Plus m, Switch m) => m AST
form = foldr (<+>) zero [tsymbol, tnumber, tstring, application, special]

parser :: (MonadState [Token] m, Plus m, Switch m) => m [AST]
parser = many0 form <* end


type Parse t a = StateT [t] Maybe a 

runParser :: Parse t a -> [t] -> Maybe (a, [t])
runParser p xs = runStateT p xs
