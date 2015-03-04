#lang eopl


;;;Lexical configuration for the problem
(define lexical
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (arith-ops ((or "+" "-" "*" "/")) symbol)
    (comp-ops ((or "<" ">" "=")) symbol)
    (true-val ("true") symbol)
    (false-val ("false") symbol)
    (undefined-val ("undefined") symbol)))


;;;Grammar for the problem. 
(define grammar
  '((expression ( "let" (arbno ID "=" expression) "in" expression) let-expr)
    (expression ( "letrec" (arbno ID "=" expression) "in" expression) letrec-expr)
    (expression ( "proc" "(" (arbno ID) ")" expression) proc-exor)
    (expression ( "(" expression (arbno expression) ")") exp-expr)
    (expression ( "newref" "(" expression ")" ) newref-expr)
    (expression ( "set" ID expression) set-expr)
    (expression ( "begin" expression (arbno ";" expression) "end") begin-expr)
    (expression ( "if" expression "then" expression "else" expression) if-then-else-expr)
    (expression ( arith-ops "(" expression (arbno "," expression) ")") arith-expr)
    (expression ( comp-ops "(" expression "," expression ")") comp-expr)
    (expression ( number) num-expr)
    (expression ( ID) id-expr)
    ;;Boolean True and False here
    (expression (true-val) true-expr)
    (expression (false-val) false-expr)
    (expression (undefined-val) undefined-expr)
    (ID (identifier) id-identifier)))
 