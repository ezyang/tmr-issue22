{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts       
             , FlexibleInstances    
             , FunctionalDependencies
             , UndecidableInstances      #-}

import Control.Monad.State       (MonadState (..), StateT(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative       (Applicative(..), Alternative(..))
import Data.List                 (nub)

-- the prelude

type Parse e t a = StateT [t] (MaybeT (Either e)) a

runParser :: Parse e t a -> [t] -> Either e (Maybe (a, [t]))
runParser p xs = runMaybeT (runStateT p xs)

many0 = many
many1 = some

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


class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

instance Functor m => Switch (MaybeT m) where
  switch (MaybeT m) = MaybeT (fmap switch m)

not1 :: (MonadState [t] m, Alternative m, Switch m) => m a -> m t
not1 p = switch p *> item

end :: (MonadState [t] m, Alternative m, Switch m) => m ()
end = switch item


class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
  throwError               =  Left
  catchError  (Right x) _  =  Right x
  catchError  (Left e)  f  =  f e
  
instance MonadError e m => MonadError e (StateT s m) where
  throwError      =  lift . throwError
  catchError m f  =  StateT (\s -> catchError (runStateT m s) (\e -> runStateT (f e) s))

instance MonadError e m => MonadError e (MaybeT m) where
  throwError      =  lift . throwError
  catchError m f  =  MaybeT $ catchError (runMaybeT m) (runMaybeT . f)

commit :: (MonadError e m, Alternative m) => e -> m a -> m a
commit err p = p <|> throwError err


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

-- the actual parser


type Error = (String, Token)


character :: (MonadState [Token] m, Alternative m) => Char -> m Token
character c = satisfy ((==) c . chr)

whitespace = many1 $ satisfy (flip elem " \n\t\r\f" . chr)
comment = pure (:) <*> character ';' <*> many0 (not1 $ character '\n')

munch p = many0 (whitespace <|> comment) *> p

opencurly  = munch $ character '{'
closecurly = munch $ character '}'
openparen  = munch $ character '('
closeparen = munch $ character ')'
symbol = munch $ many1 char
  where char = fmap chr $ satisfy (flip elem (['a' .. 'z'] ++ ['A' .. 'Z']) . chr)

eAppOper    =  "application: missing operator"
eAppClose   =  "application: missing close parenthesis"
eDefSym     =  "define: missing symbol"
eDefForm    =  "define: missing form"
eDefClose   =  "define: missing close curly"
eLamParam   =  "lambda: missing parameter list"
eLamDupe    =  "lambda: duplicate parameter names"
eLamPClose  =  "lambda: missing parameter list close curly"
eLamBody    =  "lambda: missing body form"
eLamClose   =  "lambda: missing close curly"
eSpecial    =  "special form: unable to parse"
eWoof       =  "woof: unparsed input"
-- other possibilities:  non-symbol in parameter list

application =
    openparen                             >>= \open ->
    commit (eAppOper, open) form          >>= \op ->
    many0 form                            >>= \args ->
    commit (eAppClose, open) closeparen   >>
    return (AApp op args)
    
define =
    opencurly                            >>= \open ->
    check (== "define") symbol            *>
    pure ADefine                         <*>
    commit (eDefSym, open) symbol        <*>
    commit (eDefForm, open) form         <*
    commit (eDefClose, open) closecurly  
    
lambda =
    opencurly                                >>= \open ->
    check (== "lambda") symbol               >>
    commit (eLamParam, open) opencurly       >>= \p_open ->
    many0 symbol                             >>= \params ->
    (if distinct params 
        then return ()
        else throwError (eLamDupe, p_open))  >>
    commit (eLamPClose, p_open) closecurly   >>
    commit (eLamBody, open) (many1 form)     >>= \bodies ->
    commit (eLamClose, open) closecurly      >>
    return (ALambda params bodies)
  where
    distinct names = length names == length (nub names)

special = define <|> lambda <|> (opencurly >>= \o -> throwError (eSpecial, o))

form = fmap ASymbol symbol <|> application <|> special

endCheck = 
    many0 (whitespace <|> comment) *>
    get >>= \xs -> case xs of
                        (t:_) -> throwError (eWoof, t)
                        []    -> pure ()

woof = many0 form <* endCheck
