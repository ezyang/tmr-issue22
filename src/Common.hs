module Common (

    AST(..)
  , Token
  , chr
  , line
  , col
  , countLineCol
  , simpleExample
  , example
  
) where


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
    f ((line, col), ts) '\n' = ((line + 1, 1), ('\n', line, col):ts)
    f ((line, col), ts)  c   = ((line, col + 1), (c, line, col):ts)


simpleExample = "{define \n\
\  f \n\
\  {lambda {x y}\n\
\    (plus x y)}}\n\
\\n\
\(a b (c d e))\n\
\\n\
\; here's a nice comment !!\n\
\\n\
\"


example = "{define \n\
\  f \n\
\  {lambda {x y}\n\
\    (+ x y)}}\n\
\\n\
\(* 3 (/ 4 1))\n\
\\n\
\; here's a nice comment !!\n\
\\n\
\{define\n\
\  n\n\
\  ; a very important number\n\
\  22}\n\
\"
