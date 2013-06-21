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
    openparen                           >>
    getState                            >>= \p1 -> 
    commit (eAppOper, p1) form          >>= \op ->
    many0 form                          >>= \args ->
    getState                            >>= \p2 ->
    commit (eAppClose, p2) closeparen   >>
    return (AApp op args)
    
define =
    check (== "define") symbol          >>
    getState                            >>= \p1 ->
    commit (eDefSym, p1) symbol         >>= \s  ->
    getState                            >>= \p2 ->
    commit (eDefForm, p2) form          >>= \f  ->
    getState                            >>= \p3 ->
    commit (eDefClose, p3) closecurly   >>
    return (ADefine s f)
    
lambda =
    check (== "lambda") symbol             >>
    getState                               >>= \p1 ->
    commit (eLamParam, p1) opencurly       >>
    getState                               >>= \p2 ->
    many0 symbol                           >>= \params ->
    (if distinct params 
        then return ()
        else throwError (eLamDupe, p2))    >>
    getState                               >>= \p3 ->
    commit (eLamPClose, p3) closecurly     >>
    getState                               >>= \p4 ->
    commit (eLamBody, p4) (many1 form)     >>= \bodies ->
    getState                               >>= \p5 ->
    commit (eLamClose, p5) closecurly      >>
    return (ALambda params bodies)
  where
    distinct names = length names == length (nub names)

special = 
    opencurly                       *>
    getState                       >>= \p1 ->  
    commit (eSpecial, p1) spForm
  where spForm = define <|> lambda

form = fmap ASymbol symbol <|> application <|> special

endCheck = switch item

woof = junk *> many0 form <* (getState >>= \p1 -> commit (eWoof, p1) endCheck)
