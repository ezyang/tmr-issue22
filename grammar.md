Woof         :=   Form(+)

Form         :=   Special  |  Application  |  Symbol   |  
                  String   |  Number
           
Special      :=  '{'  ( Define  |  Lambda )  '}'

Define       :=  'define'  String(?)  Symbol  Form

Lambda       :=  'lambda'  '{'  Symbol(*)  '}'  Form(+)

Application  :=  '('  Form(+)  ')'

Number       :=  \d(+)

Symbol       :=  ( \w  |  Schar)  ( \w  |  \d  |  Schar)(*)

Schar        :=  (oneof "<>!@#$%^&*_-+=|:?")

String       :=  '"'  ( Escape  |  (not  ( '"'  |  '\')))(*)  '"'

Escape       :=  '\'  ( '"'  |  '\' )

-- whitespace and comments may appear in any amount before any token.
--   tokens are:  {, }, (, ), Symbol, String, Number
Whitespace  :=  \s+

Comment     :=  ';'  (not '\n')(*)


-- errors:
--  1. redefined symbol
--  2. duplicate parameter names
--  3. unclosed application
--  4. unclosed special
--  5. bad define/lambda syntax
--  6. empty application i.e. '()'
--  7. non-comma-separated parameter names
-- errors:
--   1. unclosed string
--   2. unrecognized char
--   3. invalid escape



example:

    {define x 3}
    ;; now x is defined

    {define 
      "increment a number by 1"  ;; <-- that there's a docstring
      inc
      {lambda {x} (+ x 1)}}
        
    ({lambda {x y} (+ y (- x 3))} (+ x 2) x)
    ;;
    (eq? (take 2 "abcd\"\\e") "ab")
