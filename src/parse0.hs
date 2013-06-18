import Control.Applicative  (Applicative(..), Alternative(..))

newtype Parser t a = 
    Parser {getParser :: [t] -> Maybe ([t], a)}

item :: Parser t t
item = Parser f
  where 
    f [] = Nothing
    f (x:xs) = Just (xs, x)

instance Functor (Parser t) where
  fmap f (Parser p) = Parser (fmap (fmap f) . p)
  
instance Applicative (Parser t) where
  pure x = Parser (\xs -> pure (xs, x))
  Parser p1 <*> Parser p2 = Parser p3
    where p3 xs = case (p1 xs) of
                       Nothing      -> Nothing;
                       Just (ys, f) -> fmap (fmap f) (p2 ys)

instance Monad (Parser t) where
  return x = Parser (\ts -> return (ts, x))
  Parser p >>= f = Parser (\ts ->
                             p ts >>= \(ts', x) ->
                             getParser (f x) ts')

instance Alternative (Parser t) where
  empty = Parser (const empty)
  Parser l <|> Parser r = Parser (\ts -> l ts <|> r ts)

-- similar to mfilter, but for Alternative instead of MonadPlus
check :: (Monad f, Alternative f) => (a -> Bool) -> f a -> f a
check p m =
    m >>= \x -> if p x then return x else empty

literal :: Eq t => t -> Parser t t
literal x = check (== x) item


data Nesting
    = One Char
    | Many [Nesting]
  deriving (Show, Eq)
  
char :: Parser Char Nesting
char = fmap One $ check (not . flip elem "()") item

level :: Parser Char Nesting
level = literal '(' *> (fmap Many $ many element) <* literal ')'

element :: Parser Char Nesting
element = char <|> level

parseNest :: Parser Char [Nesting]
parseNest = many element
