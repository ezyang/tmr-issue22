import Control.Applicative  (Applicative(..))

class Applicative f => Plus f where
  zero :: f a
  (<+>) :: f a -> f a -> f a

many0 :: Plus f => f a -> f [a]
many0 p = many1 p <+> pure []

many1 :: Plus f => f a -> f [a]
many1 p = pure (:) <*> p <*> many0 p

instance Plus Maybe where
  zero = Nothing
  Nothing <+> y = y
  x       <+> _ = x


-- newtype Parser t a = Parser {getParser :: [t] -> Maybe ([t], a)}
type Parser t a = [t] -> Maybe ([t], a)

item :: Parser t t
item [] = Nothing
item (x:xs) = Just (xs, x)

check :: (a -> Bool) -> Parser t a -> Parser t a
check f p xs =
    p xs >>= \(xs', x) ->
    if f x
       then Just (xs', x)
       else Nothing

satisfy :: (t -> Bool) -> Parser t t
satisfy f = check f item

literal :: Eq t => t -> Parser t t
literal x = satisfy (== x)

(<|>) :: (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
l <|> r = \x -> l x <+> r x
