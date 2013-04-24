{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts           #-}

import Control.Monad.State       (MonadState (..), MonadPlus  (..), StateT(..), State)
import Control.Monad.Trans.Maybe (MaybeT     (..))
import Control.Applicative       (Applicative(..), Alternative(..), liftA2)
import Data.Foldable             (Foldable   (..))
import Data.Traversable          (Traversable(..))
import Data.Functor.Identity     (Identity   (..))



{-
Functor:      fmap
Applicative:  pure,      <*>,  *>,     <*
Alternative:  empty,     <|>,  many1,  many0,  guard
Monad:        (return),  >>=
*Switch*:     switch a.k.a not0,  not1
Traversable:  traverse/sequenceA  -- of *List*, not of Parser
-}


newtype Parser0 t a = Parser0 {getParser0 :: [t] -> Maybe ([t], a)}

item' :: Parser0 t t
item' = Parser0 (\xs -> case xs of
                             (t:ts) -> Just (ts, t);
                             []     -> Nothing)
                             
literal' :: Eq t => t -> Parser0 t t
literal' x = satisfy' (== x)

check' :: (a -> Bool) -> Parser0 t a -> Parser0 t a
check' f p =
    p >>= \x ->
    if (f x) then return x
             else empty

satisfy' :: (t -> Bool) -> Parser0 t t
satisfy' = flip check' item'

optional :: Alternative f => a -> f a -> f a
optional x p = p <|> pure x

optionalM :: (Functor f, Alternative f) => f a -> f (Maybe a)
optionalM p = fmap Just p <|> pure Nothing

end :: Parser0 t ()
end = switch item'

not1' :: Parser0 t a -> Parser0 t t
not1' p = switch p *> item'

sepBy1 :: Alternative f => f a -> f b -> f ([a], [b])
sepBy1 p s = pure f <*> p <*> many (liftA2 (,) s p)
  where
    f p1 rest = (p1 : map snd rest, map fst rest)

sepBy0 :: Alternative f => f a -> f b -> f ([a], [b])
sepBy0 p s = sepBy1 p s <|> pure ([], [])

string' :: Eq t => [t] -> Parser0 t [t]
string' = sequenceA . map literal'


instance Functor (Parser0 t) where
  fmap f (Parser0 p) = Parser0 (fmap (fmap f) . p)
  
instance Applicative (Parser0 t) where
  pure = return
  f <*> x = 
      f  >>=  \f' -> 
      x  >>=  \x' -> 
      return (f' x')

instance Monad (Parser0 t) where
  return x = Parser0 (\xs -> Just (xs, x))
  (Parser0 p) >>= f = 
      Parser0 (\xs -> p xs >>= \(xs', p') -> 
                      getParser0 (f p') xs')

instance Alternative (Parser0 t) where
  empty = Parser0 (const Nothing)
  Parser0 f <|> Parser0 g = Parser0 (\xs -> f xs <|> g xs)
  
class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch Nothing  =  Just ()
  switch (Just _) =  Nothing
  
instance Switch (Parser0 t) where
  switch (Parser0 p) = Parser0 (\xs -> fmap (const (xs, ())) . switch $ p xs)


data Token
    = TNumber Integer
    | TString String
    | TSymbol String
    | TOCurly 
    | TCCurly
    | TOParen
    | TCParen
    | TComma
    | TWhitespace String
    | TComment String
  deriving (Show)
  
data AST
    = ANumber Integer
    | ASymbol String
    | AString String
    | ALambda [String] [AST]
    | ADefine (Maybe String) String AST
  deriving (Show)

tnumber' :: Parser0 Char Token
tnumber' = fmap (TNumber . read) (some digit)
  where
    digit = satisfy' (\x -> elem x ['0' .. '9'])

tstring' :: Parser0 Char Token
tstring' = dq *> fmap TString (many (escape <|> regchar)) <* dq
  where
    dq = literal' '"'
    slash_or_dq = literal' '\\' <|> dq
    escape = literal' '\\' *> slash_or_dq
    regchar = not1' slash_or_dq

tsymbol' :: Parser0 Char Token
tsymbol' = fmap TSymbol (pure (:) <*> open <*> many rest)
  where 
    open = satisfy' (\x -> elem x (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "!@#$%^&*_-+=:<>?"))
    rest = open <|> satisfy' (\x -> elem x ['0' .. '9'])

tocurly' :: Parser0 Char Token
tocurly' = literal' '{' *> pure TOCurly

tccurly' :: Parser0 Char Token
tccurly' = literal' '}' *> pure TCCurly

toparen' :: Parser0 Char Token
toparen' = literal' '(' *> pure TOParen

tcparen' :: Parser0 Char Token
tcparen' = literal' ')' *> pure TCParen

tcomma' :: Parser0 Char Token 
tcomma' = literal' '\'' *> pure TComma

twhitespace' :: Parser0 Char Token
twhitespace' = fmap TWhitespace $ some (satisfy' (\x -> elem x " \t\r\f\n"))

tcomment' :: Parser0 Char Token
tcomment' = fmap TComment (literal' ';' *> some (not1' twhitespace'))

scanner' :: Parser0 Char [Token]
scanner' = many $ Prelude.foldr (<|>) empty tokens
  where
    tokens = [tnumber', tsymbol', tstring', 
              tocurly', tccurly', toparen',
              tcparen', twhitespace', 
              tcomma',  tcomment']

eg1 = "{define $#xyz123 (+ 123 \"I'm a string\")}"

runParser0 = getParser0 scanner' eg1
  
-- didn't do:  commit



{- 
language to parse:  Beagle + check that variables
  are defined before use and not defined > 1x per
  scope
  maybe also put commas between list items
-}

item :: (MonadState [t] m, MonadPlus m) => m t
item =
    get >>= \xs -> case xs of
                        (t:ts) -> put ts >> return t;
                        []     -> mzero;

-- [t] -> Maybe ([t], a)
type Parse1a t a = StateT [t] Maybe a 

-- m (Maybe a) + (s -> (s, a)) == (s -> (s, Maybe a))
type Parse1b t a = MaybeT (State [t]) a

instance Show a => Show (Identity a) where
  show (Identity x) = show x
  
