module Classes (

    Plus(..)
  , many0
  , many1
  , Switch(..)
  , Either'(..)
  
) where

import Control.Applicative  (Applicative(..))


class Applicative f => Plus f where
  zero :: f a
  (<+>) :: f a -> f a -> f a


many0 :: Plus f => f a -> f [a]
many0 p = many1 p <+> pure []

many1 :: Plus f => f a -> f [a]
many1 p = pure (:) <*> p <*> many0 p


class Switch f where
  switch :: f a -> f ()


data Either' a b
    = Left' a
    | Right' b
  deriving (Show, Eq)
