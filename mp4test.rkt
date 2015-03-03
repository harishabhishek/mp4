#lang eopl


;;;Lexical configuration for the problem
(define lexical
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression ( "let" (arbno ID "=" expression) "in" expression) let-expr)
    (expression ( "letrec" (arbno ID "=" expression) "in" expression) letrec-expr)
    (expression ( "proc" "(" (arbno ID) ")" expression) proc-exor)
    (expression ( "(" expression (arbno expression) ")") exp-expr)
    (expression ( "newref" "(" expression ")" ) newref-expr)
    (expression (New line
 