Woof BNF-like grammar:

    Woof         :=   Form(+)

    Form         :=   Symbol  |  Special  |  Application

    Symbol       :=   [a-zA-Z](+)

    Special      :=   '{'  ( Define  |  Lambda )  '}'

    Define       :=   'define'  Symbol  Form

    Lambda       :=   'lambda'  '{'  Symbol(*)  '}'  Form(+)

    Application  :=   '('  Form(+)  ')'

Whitespace and comments may appear in any amount before any token.
Tokens are:  `{`, `}`, `(`, `)`, and `Symbol`.

    Whitespace   :=   \s+

    Comment      :=   ';'  (not '\n')(*)
