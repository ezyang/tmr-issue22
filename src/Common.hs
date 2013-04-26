module Common (

    AST(..)
  , Token
  , chr
  , line
  , col
  , countLineCol
  
) where


data AST
    = ANumber Integer
    | ASymbol String
    | AString String
    | ALambda [String] [AST]
    | ADefine (Maybe String) String AST
    | AApp    AST  [AST]
  deriving (Show, Eq)


type Token = (Char, Int, Int)
chr  (a, _, _)  =  a
line (_, b, _)  =  b
col  (_, _, c)  =  c

countLineCol :: [Char] -> [Token]
countLineCol = reverse . snd . foldl f ((1, 1), [])
  where
    f ((line, col), ts) '\n' = ((line + 1, 1), ('\n', line, col):ts)
    f ((line, col), ts)  c   = ((line, col + 1), (c, line, col):ts)

