Woof  :=   Form(+)

Form  :=   Special  |  Application  |  
           SYMBOL   |  STRING       |
           NUMBER
           
Special  :=  OCURLY  ( Define  |  Lambda )  CCURLY
-- leave Cond and List as exercise for reader

Define   :=  SYMBOL('define')  String(?)  SYMBOL  Form
-- allow optional docstring
-- can define symbol once per scope

Lambda   :=  OCURLY  (sepBy0 SYMBOL COMMA)  CCURLY  Form(+)
-- and make lambda's symbols unique

Application  :=  OPAREN  Form(+)  CPAREN

-- errors:
--  1. redefined symbol
--  2. duplicate parameter names
--  3. unclosed application
--  4. unclosed special
--  5. bad define/lambda syntax
--  6. empty application i.e. '()'
--  7. non-comma-separated parameter names


-- Tokens

NUMBER  :=  \d+

SYMBOL  :=  ( \w  |  schar)  ( \w  |  \d  |  schar)(*)

schar   :=  (oneof "<>!@#$%^&*_-+=|:?")

STRING  :=  '"'  ( escape  |  (not  ( '"'  |  '\')))(*)  '"'

escape  :=  '\'  ( '"'  |  '\' )

COMMA   :=  ','

OCURLY  :=  '{'

CCURLY  :=  '}'

OPAREN  :=  '('

CPAREN  :=  ')'

WHITESPACE  :=  \s+

COMMENT     :=  ';'  (not '\n')(*)
-- extension to other newline characters left as exercise for reader

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

