{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..), State)
import Control.Monad.Error       (MonadError (..), ErrorT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Applicative       (Applicative(..))
import Data.List                 (nub)
import Data.Functor.Identity     (Identity(..))
import Parser
import Classes
import Instances
import Common


-- other effects to try:
--   partial results
--   whitespace/comment reporting

-- list of already seen forms,
-- list of text, line, column
--   for whitespace/comment
type Log = [(String, Int, Int)]

logJunk text line col =
    lift get              >>= \junks ->
    lift $ put ((text, line, col):junks)

type Error = (String, Token)

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
    
string = munch (dq          >>= \open ->
                many0 char  <* 
                commit ("unclosed string literal", open) dq)
  where
    dq = character '"'
    char = fmap chr (escape <+> normal)
    escape = character '\\' *> slash_or_dq
    normal = not1 slash_or_dq
    slash_or_dq = character '\\' <+> character '"'


whitespace = 
    (many1 $ satisfy (flip elem " \n\t\r\f" . chr)) >>= \toks ->
    let text = map chr toks
        lin  = line (head toks)
        coln = col  (head toks)
    in
        logJunk text lin coln

comment = 
    character ';'                     >>= \op ->
    many0 (not1 $ character '\n')     >>= \rest ->
--    return ()
    logJunk (map chr rest) (line op) (col op)

munch p = many0 (whitespace <+> comment) *> p

top = munch oparen
tcp = munch cparen
toc = munch ocurly
tcc = munch ccurly

-- tsymbol :: (MonadState [Token] m, Plus m, Switch m) => m AST
tsymbol = fmap ASymbol symbol

-- tstring :: (MonadState [Token] m, Plus m, Switch m) => m AST
tstring = fmap AString string

-- tnumber :: (MonadState [Token] m, Plus m, Switch m) => m AST
tnumber = fmap (ANumber . read) number

-- application :: (MonadState [Token] m, Plus m, Switch m) => m AST
application =
    top            >>= \open ->
    operator open  >>= \op ->
    many0 form     >>= \args ->
    close open     >>
    return (AApp op args)
  where
    operator open = commit ("missing application operator", open) form
    close open = commit ("missing application close", open) tcp
    
lambda =
    check (== "lambda") symbol  >>
    toc                         >>= \open ->
    pars open                   >>= \params ->
    close open                  >>
    body open                   >>= \bodies ->
    return (ALambda params bodies)
  where
    pars open = 
        many0 symbol >>= \them -> 
        if distinct them then return them
                         else throwError ("duplicate parameter names", open)
    distinct names = length names == length (nub names)
    close open = commit ("missing parameter list close", open) tcc
    body open = commit ("missing 1 or more lambda bodies", open) (many1 form)

define open =
    check (== "define") symbol    *>
    pure ADefine                 <*>
    optionalM string             <*>
    sym                          <*>
    for
  where
    sym = commit ("define: missing symbol", open) symbol
    for = commit ("define: missing form", open) form
    
special = 
    toc          >>= \open ->
    spec open    >>= \val ->
    close open   >>
    return val
  where
    spec open = commit ("unable to parse special form", open) (lambda <+> define open)
    close open = commit ("missing special form close", open) tcc
    
form = foldr (<+>) zero [tsymbol, tnumber, tstring, application, special]

parser = many0 form <* endCheck
  where
    endCheck = 
        many0 (whitespace <+> comment) *>
        get >>= \xs -> case xs of
                            (t:_) -> throwError ("unparsed input", t)
                            []    -> pure ()

runParse :: Parse s e t a -> [t] -> s -> Either' e (Maybe ((a, [t]), s))
runParse p xs s = runMaybeT (runStateT (runStateT p xs) s)

type Parse s e t a = StateT [t] (StateT s (MaybeT (Either' e))) a
