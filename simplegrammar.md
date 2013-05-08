Woof         :=   Form(+)

Form         :=   Symbol  |  Special  |  Application

Symbol       :=  [a-zA-Z](+)

Special      :=  '{'  ( Define  |  Lambda )  '}'

Define       :=  'define'  Symbol  Form

Lambda       :=  'lambda'  '{'  Symbol(*)  '}'  Form(+)

Application  :=  '('  Form(+)  ')'

-- whitespace and comments may appear in any amount before any token.
--   tokens are:  {, }, (, ), Symbol
Whitespace   :=  \s+

Comment      :=  ';'  (not '\n')(*)
