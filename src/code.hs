import Control.Applicative       (Applicative(..), Alternative(..))
import Data.Foldable             (Foldable   (..))
import Data.Traversable          (Traversable(..))
import Common



newtype Parser' t a = Parser' {getParser' :: [t] -> Maybe ([t], a)}

item' :: Parser' t t
item' = Parser' (\xs -> case xs of
                             (t:ts) -> Just (ts, t);
                             []     -> Nothing)
                             
literal' :: Eq t => t -> Parser' t t
literal' x = satisfy' (== x)

check' :: (a -> Bool) -> Parser' t a -> Parser' t a
check' f p =
    p >>= \x ->
    if (f x) then return x
             else empty

satisfy' :: (t -> Bool) -> Parser' t t
satisfy' = flip check' item'

end' :: Parser' t ()
end' = switch item'

not1' :: Parser' t a -> Parser' t t
not1' p = switch p *> item'

string' :: Eq t => [t] -> Parser' t [t]
string' = sequenceA . map literal'


instance Functor (Parser' t) where
  fmap f (Parser' p) = Parser' (fmap (fmap f) . p)
  
instance Applicative (Parser' t) where
  pure = return
  f <*> x = 
      f  >>=  \f' -> 
      x  >>=  \x' -> 
      return (f' x')

instance Monad (Parser' t) where
  return x = Parser' (\xs -> Just (xs, x))
  (Parser' p) >>= f = 
      Parser' (\xs -> p xs >>= \(xs', p') -> 
                      getParser' (f p') xs')

instance Alternative (Parser' t) where
  empty = Parser' (const Nothing)
  Parser' f <|> Parser' g = Parser' (\xs -> f xs <|> g xs)
  
class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch Nothing  =  Just ()
  switch (Just _) =  Nothing
  
instance Switch (Parser' t) where
  switch (Parser' p) = Parser' (\xs -> fmap (const (xs, ())) . switch $ p xs)


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
  deriving (Show, Eq)
  
data AST
    = ANumber Integer
    | ASymbol String
    | AString String
    | ALambda [String] [AST]
    | ADefine (Maybe String) String AST
  deriving (Show, Eq)

tnumber' :: Parser' Char Token
tnumber' = fmap (TNumber . read) (some digit)
  where
    digit = satisfy' (\x -> elem x ['0' .. '9'])

tstring' :: Parser' Char Token
tstring' = dq *> fmap TString (many (escape <|> regchar)) <* dq
  where
    dq = literal' '"'
    slash_or_dq = literal' '\\' <|> dq
    escape = literal' '\\' *> slash_or_dq
    regchar = not1' slash_or_dq

tsymbol' :: Parser' Char Token
tsymbol' = fmap TSymbol (pure (:) <*> open <*> many rest)
  where 
    open = satisfy' (\x -> elem x (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "!@#$%^&*_-+=:<>?"))
    rest = open <|> satisfy' (\x -> elem x ['0' .. '9'])

tocurly' :: Parser' Char Token
tocurly' = literal' '{' *> pure TOCurly

tccurly' :: Parser' Char Token
tccurly' = literal' '}' *> pure TCCurly

toparen' :: Parser' Char Token
toparen' = literal' '(' *> pure TOParen

tcparen' :: Parser' Char Token
tcparen' = literal' ')' *> pure TCParen

tcomma' :: Parser' Char Token 
tcomma' = literal' '\'' *> pure TComma

twhitespace' :: Parser' Char Token
twhitespace' = fmap TWhitespace $ some (satisfy' (\x -> elem x " \t\r\f\n"))

tcomment' :: Parser' Char Token
tcomment' = fmap TComment (literal' ';' *> some (not1' twhitespace'))

scanner' :: Parser' Char [Token]
scanner' = (many $ Prelude.foldr (<|>) empty tokens) <* end'
  where
    tokens = [tnumber', tsymbol', tstring', 
              tocurly', tccurly', toparen',
              tcparen', twhitespace', 
              tcomma',  tcomment']

eg = "{define $#xyz123 (+ 123 \"I'm a string\")}"

runScanner' = getParser' scanner'


-- Parser' t a -> (a -> Maybe b) -> Parser' t b
asymbol' :: Parser' Token AST
asymbol' = item' >>= f
  where
    f (TSymbol s) = pure $ ASymbol s
    f _           = empty

anumber' :: Parser' Token AST
anumber' = item' >>= f
  where
    f (TNumber n) = pure $ ANumber n
    f    _        = empty
    
astring' :: Parser' Token AST
astring' = item' >>= f
  where
    f (TString s) = pure $ AString s
    f      _      = empty
    
adefine' :: Parser' Token AST
adefine' = 
    literal' (TSymbol "define")   *>
    pure ADefine                 <*>
    asymbol'                     <*>
    aform'

alambda' :: Parser' Token AST
alambda' = 
    literal' (TSymbol "lambda")   *>
    pure ALambda                 <*>
    literal' TOCurly              *>
    many asymbol'                <*>
    literal' TCCurly             <*
    many aform'
    
aspecial' :: Parser' Token AST
aspecial' = 
    literal' TOCurly          *>
    (adefine' <|> alambda')  <*
    literal' TCCurly
