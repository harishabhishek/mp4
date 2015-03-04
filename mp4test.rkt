#lang eopl

;;;Errors
(define (report-invalid-syntax val)
  ((eopl:error 'parse-exp "Invalid syntax ~s" val)))

;;;Lexical configuration for the problem
(define lexical
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (arith-ops ((or "+" "-" "*" "/")) symbol)
    (comp-ops ((or "<" ">" "=")) symbol)
    (true-val ("true") symbol)                             ;does this need to be "#t"?
    (false-val ("false") symbol)                           ;does this need to be "#f"?
    (undefined-val ("undefined") symbol)))                 ;does this need to be "'undefined"?


;;;Grammar for the problem. 
(define grammar
  '((expression ( "let" (arbno ID "=" expression) "in" expression) let-expr)
    (expression ( "letrec" (arbno ID "=" expression) "in" expression) letrec-expr)
    (expression ( "proc" "(" (arbno ID) ")" expression) proc-expr)                ;fixed a typo
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
 
;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
(sllgen:make-define-datatypes lexical grammar)
  
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexical grammar)))
  
(define scan&parse
  (sllgen:make-string-parser lexical grammar))
  
(define just-scan
  (sllgen:make-string-scanner lexical grammar))

(define static-interpreter 
  (lambda (input)
    (value-of-static (scan&parse input))))

(define value-of-static 
  (lambda (parsed-struct) 
    (cases expression parsed-struct
      (true-expr (value) (display "do something about true-val"))
      (false-expr (value) (display "do something about false-val"))
      (undefined-expr (value) (display "do something about undefined-val"))
      (num-expr (value) (display "do something about number"))
      (id-expr (value) (display "do something about ID"))
      (arith-expr (operator expr1 list-of-expr) (display "do something about arithmetic expressions"))
      (comp-expr (operator expr1 list-of-expr) (display "do something about comparitive expressions"))
      (exp-expr (expr1 list-of-expr) (display "do something about parenthesized expressions"))
      (if-then-else-expr (if-expr else-expr then-expr) (display "do something about if-then-else"))
      (let-expr (list-of-ID list-of-expr in-expr) (display "do something about let"))
      (letrec-expr (list-of-ID list-of-expr in-expr) (display "do something about letrec"))
      (proc-expr (list-of-ID expr) (display "do something about procedures"))
      (set-expr (ID expr) (display "do something about set"))
      (begin-expr (begin-expr list-of-expr) (display "do something about begin"))
      (newref-expr (expr) (display "do something about newref"))
      (else (report-invalid-syntax parsed-struct)))))

;Now we would need to write each ones "value-of" function (or in some cases just return a value or error)
;test branch commit
