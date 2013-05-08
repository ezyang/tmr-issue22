{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..))
import Control.Monad.Error       (MonadError (..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative       (Applicative(..))
import Data.List                 (nub)
import Parser
import Classes
import Instances
import Common


type Error = (String, Token)


character :: (MonadState [Token] m, Plus m) => Char -> m Token
character c = satisfy ((==) c . chr)

whitespace = many1 $ satisfy (flip elem " \n\t\r\f" . chr)
comment = character ';' *> many0 (not1 $ character '\n')

munch p = many0 (whitespace <+> comment) *> p

ocurly = munch $ character '{'
ccurly = munch $ character '}'
oparen = munch $ character '('
cparen = munch $ character ')'
symbol = munch $ many1 char
  where char = fmap chr $ satisfy (\t -> elem (chr t) (['a' .. 'z'] ++ ['A' .. 'Z']))

eaOp   =  "application: missing operator"
eaCls  =  "application: missing close parenthesis"
edSym  =  "define: missing symbol"
edForm =  "define: missing form"
edCls  =  "define: missing close curly"
elPL   =  "lambda: missing parameter list"
elPrms =  "lambda: duplicate parameter names"
elPCls =  "lambda: missing parameter list close curly"
elBody =  "lambda: missing body form"
elCls  =  "lambda: missing close curly"
ewUnp  =  "woof: unparsed input"
esName =  "special form: unable to parse"

application =
    oparen                       >>= \open ->
    commit (eaOp, open) form     >>= \op ->
    many0 form                   >>= \args ->
    commit (eaCls, open) cparen  >>
    return (AApp op args)
    
define =
    ocurly                       >>= \open ->
    check (== "define") symbol    *>
    pure ADefine                 <*>
    commit (edSym, open) symbol  <*>
    commit (edForm, open) form   <*
    commit (edCls, open) ccurly  
    
lambda =
    ocurly                               >>= \open ->
    check (== "lambda") symbol           >>
    commit (elPL, open) ocurly           >>= \p_open ->
    many0 symbol                         >>= \params ->
    (if distinct params 
        then return ()
        else throwError (elPrms, open))  >>
    commit (elPCls, open) ccurly         >>
    commit (elBody, open) (many1 form)   >>= \bodies ->
    commit (elCls, open) ccurly          >>
    return (ALambda params bodies)
  where
    distinct names = length names == length (nub names)

special = define <+> lambda <+> (ocurly >>= \o -> throwError (esName, o))

form = fmap ASymbol symbol <+> application <+> special

endCheck = 
    many0 (whitespace <+> comment) *>
    get >>= \xs -> case xs of
                        (t:_) -> throwError (ewUnp, t)
                        []    -> pure ()

woof = many0 form <* endCheck

runParser :: Parse e t a -> [t] -> Either' e (Maybe (a, [t]))
runParser p xs = runMaybeT (runStateT p xs)

type Parse e t a = StateT [t] (MaybeT (Either' e)) a
