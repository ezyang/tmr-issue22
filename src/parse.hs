{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), StateT(..), State)
import Control.Applicative       (Applicative(..), Alternative(..), liftA2)
import Data.List                 (nub)
-- import Control.Monad.Trans.Maybe (MaybeT     (..))
-- import Data.Functor.Identity     (Identity   (..))

class Switch f where
  switch :: f a -> f ()
  
not1 p = switch p *> item

data AST
    = ANumber Integer
    | ASymbol String
    | AString String
    | ALambda [String] [AST]
    | ADefine (Maybe String) String AST
    | AApp    AST  [AST]
  deriving (Show, Eq)

type Token = (Char, Int, Int)
chr  (a, _, _)  =  a
line (_, b, _)  =  b
col  (_, _, c)  =  c

countLineCol :: [Char] -> [Token]
countLineCol = reverse . snd . foldl f ((1, 1), [])
  where
    f ((line, col), ts) '\n' = ((line + 1, 1), ('\n', line, col):ts)
    f ((line, col), ts)  c   = ((line, col + 1), (c, line, col):ts)


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

character :: (MonadState [Token] m, Alternative m) => Char -> m Token
character c = satisfy ((==) c . chr)

ocurly = character '{'

ccurly = character '}'

oparen = character '('

cparen = character ')'

digit = fmap chr $ satisfy (flip elem ['0' .. '9'] . chr)

number = munch $ some digit

symbolOpens = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "!@#$%^&*_-+=:?<>"

symbol = munch (fmap (:) first <*> rest)
  where
    first = fmap chr $ satisfy (flip elem symbolOpens . chr)
    rest = many (first <|> digit)
    
instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()
  
instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

string = munch (dq *> many char <* dq)
  where
    dq = character '"'
    char = fmap chr (escape <|> normal)
    escape = character '\\' *> slash_or_dq
    normal = not1 slash_or_dq
    slash_or_dq = character '\\' <|> character '"'


whitespace = some $ satisfy (flip elem " \n\t\r\f" . chr)

comment = character ';' *> many (not1 $ character '\n')

munch p = many (whitespace <|> comment) *> p

top = munch oparen
tcp = munch cparen
toc = munch ocurly
tcc = munch ccurly

tsymbol = fmap ASymbol symbol

tstring = fmap AString string

tnumber = fmap (ANumber . read) number

application =
    top        >>= \open ->
    form       >>= \op ->
    many form  >>= \args ->
    tcp        >>
    return (AApp op args)
    
lambda :: (MonadState [Token] m, Alternative m, Switch m) => m AST
lambda =
    check (== "lambda") symbol    >>
    toc                           >>
    check distinct (many symbol)  >>= \params ->
    some form                     >>= \bodies ->
    tcc                           >>
    return (ALambda params bodies)
  where
    distinct names = length names == length (nub names)

optionalM :: (Functor f, Alternative f) => f a -> f (Maybe a)
optionalM p = fmap Just p <|> pure Nothing

define :: (MonadState [Token] m, Alternative m, Switch m) => m AST
define =
    check (== "define") symbol   *>
    pure ADefine                <*>
    optionalM string            <*>
    symbol                      <*>
    form
    
special :: (MonadState [Token] m, Alternative m, Switch m) => m AST
special = 
    toc  >>= \open ->
    (lambda <|> define) >>= \val ->
    tcc  >>
    return val
    
form = foldr (<|>) empty [tsymbol, tnumber, tstring, application, special]

-- scanner = (many $ foldr (<|>) empty [ocurly, ccurly, oparen, cparen]) *> end


runParser :: Parse1a t a -> [t] -> Maybe (a, [t])
runParser p xs = runStateT p xs

{-
class Switch f where
  switch :: f a -> f ()
-} 

-- [t] -> Maybe (a, [t])
type Parse1a t a = StateT [t] Maybe a 

{-
-- s -> (Maybe a, s)
type Parse1b t a = MaybeT (State [t]) a

instance Show a => Show (Identity a) where
  show (Identity x) = show x
-}