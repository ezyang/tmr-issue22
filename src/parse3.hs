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


-- parser definition

type Pos = (Int, Int)
type Err = (String, Pos)

type Parser t a = StateT [t] (StateT Pos (MaybeT (Either Err))) a 

runParser :: Parser t a -> [t] -> Either Err (Maybe ((a, [t]), Pos))
runParser p ts = runMaybeT $ runStateT (runStateT p ts) (1, 1)

-- basic parser:  item

getState :: Parser Char Pos
getState = lift get

putState :: Pos -> Parser Char ()
putState = lift . put

updateState :: (Pos -> Pos) -> Parser Char ()
updateState f = getState >>= (putState . f)

basic_item :: (MonadState [t] m, Alternative m) => m t
basic_item =
    get >>= \xs -> case xs of
                        (t:ts) -> put ts *> pure t;
                        []     -> empty;

item :: Parser Char Char
item = basic_item >>= \x -> updateState (f x) >> pure x
  where f '\n' (ln, c) = (ln + 1, 1)
        f _    (ln, c) = (ln, c + 1)

-- the prelude

many0 = many
many1 = some


check :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
check f p =
    p >>= \x ->
    if (f x) then return x else empty

satisfy :: (Char -> Bool) -> Parser Char Char
satisfy = flip check item

literal :: Char -> Parser Char Char
literal c = satisfy ((==) c)


class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

instance Functor m => Switch (MaybeT m) where
  switch (MaybeT m) = MaybeT (fmap switch m)

not1 :: Parser Char a -> Parser Char Char
not1 p = switch p *> item

end :: Parser Char ()
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
  catchError m f  =  StateT g
    where
      g s = catchError (runStateT m s) 
                       (\e -> runStateT (f e) s)

instance MonadError e m => MonadError e (MaybeT m) where
  throwError      =  lift . throwError
  catchError m f  =  MaybeT $ catchError (runMaybeT m) (runMaybeT . f)

commit :: (MonadError e m, Alternative m) => e -> m a -> m a
commit err p = p <|> throwError err

-- 

data AST
    = ASymbol String
    | ALambda [String] [AST]
    | ADefine String AST
    | AApp    AST  [AST]
  deriving (Show, Eq)


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

whitespace = many1 $ satisfy (flip elem " \n\t\r\f")
comment = pure (:) <*> literal ';' <*> many0 (not1 $ literal '\n')

junk = many0 (whitespace <|> comment)

tok p = p <* junk

opencurly  = tok $ literal '{'
closecurly = tok $ literal '}'
openparen  = tok $ literal '('
closeparen = tok $ literal ')'
symbol = tok $ many1 char
  where char = satisfy (flip elem (['a' .. 'z'] ++ ['A' .. 'Z']))

eAppOper    =  "application: missing operator"
eAppClose   =  "application: missing close parenthesis"
eDefSym     =  "define: missing symbol"
eDefForm    =  "define: missing form"
eLamParam   =  "lambda: missing parameter list"
eLamDupe    =  "lambda: duplicate parameter names"
eLamPClose  =  "lambda: missing parameter list close curly"
eLamBody    =  "lambda: missing body form"
eSpecClose  =  "special form: missing close curly"
eSpecial    =  "special form: unable to parse"
eWoof       =  "woof: unparsed input"


cut :: String -> Parser Char a -> Parser Char a
cut message parser = 
    getState >>= \p ->
    commit (message, p) parser


application =
    openparen                     >>
    cut eAppOper form             >>= \op ->
    many0 form                    >>= \args ->
    cut eAppClose closeparen      >>
    return (AApp op args)

define =
    check (== "define") symbol    *>
    pure ADefine                 <*>
    cut eDefSym symbol           <*>
    cut eDefForm form

lambda =
    check (== "lambda") symbol     >>
    cut eLamParam opencurly        >>
    many0 symbol                   >>= \params ->
    (if distinct params 
        then return ()
        else cut eLamDupe empty)      >>
    cut eLamPClose closecurly         >>
    cut eLamBody (many1 form)         >>= \bodies ->
    return (ALambda params bodies)
  where
    distinct names = length names == length (nub names)

special = 
    opencurly  *>  cut eSpecial (define <|> lambda)  <*  cut eSpecClose closecurly

form = fmap ASymbol symbol <|> application <|> special

woof = junk *> many0 form <* cut eWoof end
