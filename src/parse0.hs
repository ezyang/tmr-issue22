import Control.Applicative  (Applicative(..), Alternative(..))
import Control.Monad        (liftM, ap)

newtype Parser t a = 
    Parser {getParser :: [t] -> Maybe ([t], a)}

item :: Parser t t
item = Parser f
  where 
    f [] = Nothing
    f (x:xs) = Just (xs, x)

instance Monad (Parser t) where
  return x = Parser (\ts -> Just (ts, x))
  Parser p >>= f = Parser (\ts ->
                             p ts >>= \(ts', x) ->
                             getParser (f x) ts')

instance Functor (Parser t) where
  fmap = liftM
  
instance Applicative (Parser t) where
  pure = return
  (<*>) = ap          

instance Alternative (Parser t) where
  empty = Parser (const empty)
  Parser l <|> Parser r = Parser (\ts -> l ts <|> r ts)

-- similar to mfilter, but for Alternative instead of MonadPlus
check :: (Monad f, Alternative f) => (a -> Bool) -> f a -> f a
check p m =
    m >>= \x -> if p x then return x else empty

literal :: Eq t => t -> Parser t t
literal x = check (== x) item
