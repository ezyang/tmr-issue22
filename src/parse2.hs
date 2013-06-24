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

type Parser e t a = StateT [t] (MaybeT (Either e)) a

runParser :: Parser e t a -> [t] -> Either e (Maybe (a, [t]))
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

literal :: (Eq t, MonadState [t] m, Alternative m) => t -> m t
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
  catchError m f  =  StateT g
    where
      g s = catchError (runStateT m s) 
                       (\e -> runStateT (f e) s)

instance MonadError e m => MonadError e (MaybeT m) where
  throwError      =  lift . throwError
  catchError m f  =  MaybeT $ catchError (runMaybeT m) (runMaybeT . f)

commit :: (MonadError e m, Alternative m) => e -> m a -> m a
commit err p = p <|> throwError err


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


type Error = String


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
    openparen                     >>
    commit eAppOper form          >>= \op ->
    many0 form                    >>= \args ->
    commit eAppClose closeparen   >>
    return (AApp op args)
    
define =
    check (== "define") symbol    *>
    pure ADefine                 <*>
    commit eDefSym symbol        <*>
    commit eDefForm form         <*
    commit eDefClose closecurly
    
lambda =
    check (== "lambda") symbol       >>
    commit eLamParam opencurly       >>
    many0 symbol                     >>= \params ->
    (if distinct params 
        then return ()
        else throwError eLamDupe)    >>
    commit eLamPClose closecurly     >>
    commit eLamBody (many1 form)     >>= \bodies ->
    return (ALambda params bodies)   <*
    commit eLamClose closecurly  
  where
    distinct names = length names == length (nub names)

special = 
    opencurly  *>  commit eSpecial (define <|> lambda)

form = fmap ASymbol symbol <|> application <|> special

endCheck = switch item

woof :: Parser String Char [AST]
woof = junk *> many0 form <* commit eWoof endCheck
