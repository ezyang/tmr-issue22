{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts       
             , FlexibleInstances    
             , MultiParamTypeClasses  #-}


import Control.Monad             (liftM, ap)
import Control.Monad.State       (MonadState (..), StateT(..))
import Control.Monad.Error       (MonadError (..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative       (Applicative(..))
import Data.List                 (nub)

-- the prelude

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

  
instance Plus Maybe where
  zero = Nothing
  Nothing <+> y = y
  x       <+> _ = x

instance (Functor m, Monad m) => Plus (MaybeT m) where
  zero = MaybeT (return Nothing)
  MaybeT l  <+>  MaybeT r  =  MaybeT x
    where
      x = l >>= \y -> case y of Nothing -> r;
                                Just _  -> return y;

instance (Monad m, Plus m) => Plus (StateT s m) where
  zero = StateT (const zero)
  StateT f <+> StateT g = StateT (\s -> f s <+> g s)


instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

instance Functor m => Switch (MaybeT m) where
  switch (MaybeT m) = MaybeT (fmap switch m)


instance Functor (Either' e) where
  fmap = liftM

instance Applicative (Either' e) where
  pure = return
  (<*>) = ap

instance Monad (Either' e) where
  return            =  Right'
  Left' e   >>=  _  =  Left' e
  Right' x  >>=  f  =  f x

instance MonadError e (Either' e) where
  throwError                =  Left'
  catchError  (Right' x) _  =  Right' x
  catchError  (Left' e)  f  =  f e


data AST
    = ANumber Integer
    | ASymbol String
    | AString String
    | ALambda [String] [AST]
    | ADefine String AST
    | AApp    AST  [AST]
  deriving (Show, Eq)


type Token = (Char, Int, Int)
chr  (a, _, _)  =  a
line (_, b, _)  =  b
col  (_, _, c)  =  c

countLineCol :: [Char] -> [Token]
countLineCol = reverse . snd . foldl f ((1, 1), [])
  where
    f ((l, c), ts) '\n'   = ((l + 1, 1), ('\n', l, c):ts)
    f ((l, c), ts)  char  = ((l, c + 1), (char, l, c):ts)


example = "{define \n\
\  f \n\
\  {lambda {x y}\n\
\    (plus x y)}}\n\
\\n\
\(a b (c d e))\n\
\\n\
\; here's a nice comment !!\n\
\\n\
\"

item :: (MonadState [t] m, Plus m) => m t
item =
    get >>= \xs -> case xs of
                        (t:ts) -> put ts *> pure t;
                        []     -> zero;

check :: (Monad m, Plus m) => (a -> Bool) -> m a -> m a
check f p =
    p >>= \x ->
    if (f x) then return x else zero

satisfy :: (MonadState [t] m, Plus m) => (t -> Bool) -> m t
satisfy = flip check item

not1 :: (MonadState [t] m, Plus m, Switch m) => m a -> m t
not1 p = switch p *> item

end :: (MonadState [t] m, Plus m, Switch m) => m ()
end = switch item

commit :: (MonadError e m, Plus m) => e -> m a -> m a
commit err p = p <+> throwError err

-- the actual parser


type Error = (String, Token)


character :: (MonadState [Token] m, Plus m) => Char -> m Token
character c = satisfy ((==) c . chr)

whitespace = many1 $ satisfy (flip elem " \n\t\r\f" . chr)
comment = pure (:) <*> character ';' <*> many0 (not1 $ character '\n')

munch p = many0 (whitespace <+> comment) *> p

ocurly = munch $ character '{'
ccurly = munch $ character '}'
oparen = munch $ character '('
cparen = munch $ character ')'
symbol = munch $ many1 char
  where char = fmap chr $ satisfy (flip elem (['a' .. 'z'] ++ ['A' .. 'Z']) . chr)

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
esName =  "special form: unable to parse"
ewUnp  =  "woof: unparsed input"
-- other possibilities:  non-symbol in parameter list

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
    ocurly                                 >>= \open ->
    check (== "lambda") symbol             >>
    commit (elPL, open) ocurly             >>= \p_open ->
    many0 symbol                           >>= \params ->
    (if distinct params 
        then return ()
        else throwError (elPrms, p_open))  >>
    commit (elPCls, p_open) ccurly         >>
    commit (elBody, open) (many1 form)     >>= \bodies ->
    commit (elCls, open) ccurly            >>
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


type Parse e t a = StateT [t] (MaybeT (Either' e)) a

runParser :: Parse e t a -> [t] -> Either' e (Maybe (a, [t]))
runParser p xs = runMaybeT (runStateT p xs)
