#lang eopl

;;;Errors
(define (report-invalid-syntax val)
  ((eopl:error 'parse-exp "Invalid syntax ~s" val)))

;;;Environments
;-----------------This is the code from the MP2 solution----------

;empty-env : () → Env
(define empty-env
    (lambda (search-var)
      'not-found))

;extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (saved-env saved-var saved-val)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

;apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (env search-var)))

;;;Lexical configuration for the problem
(define lexical
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (arith-ops ((or "+" "-" "*" "/")) symbol)
    (comp-ops ((or "<" ">" "=")) symbol)                 ;We may need to change this to be able to pull out the operator
    ;;;"=( kills the error, but is kind of a hack"
    (true-val ("#t") symbol)                             ;does this need to be "#t"?
    (false-val ("#f") symbol)                           ;does this need to be "#f"?
    (undefined-val ("'undefined") symbol)))                 ;does this need to be "'undefined"?


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
    ;;;I don't want to have to do this, but we may need to split this into 
    ;;;two or three different comp expressions so we can get the = 
    ;;;operator in here as Piazza post @411
    ;(expression ( comp-ops "(" expression "," expression ")") comp-expr)
    ;like this
    (expression ( "=" "(" expression "," expression ")") comp-expr-eq)
    (expression ( "<" "(" expression "," expression ")") comp-expr-lt)
    (expression ( ">" "(" expression "," expression ")") comp-expr-gt)
    ;/like this
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

;;The definition of the interpreter
(define static-interpreter 
  (lambda (input)
    ;(initialize-store!)
    (value-of-static (scan&parse input) empty-env)))
;(trace static-interpreter)

;;;Handles the separate cases of input
(define value-of-static 
  (lambda (parsed-struct env) 
    (cases expression parsed-struct
      (true-expr (value) value) ;done?
      (false-expr (value) value) ;done?
      (undefined-expr (value) value) ;done?
      (num-expr (value) value) ;done?
      (id-expr (value) (value-of-id-exp value env)) ;started
      (arith-expr (operator expr1 list-of-expr) (value-of-arith-exp operator expr1 list-of-expr env)) ;started
      ;(comp-expr (operator expr1 list-of-expr) (value-of-comp-exp operator expr1 list-of-expr env))
      (comp-expr-eq (expr1 list-of-expr) (value-of-comp-exp-eq expr1 list-of-expr env))
      (comp-expr-lt (expr1 list-of-expr) (value-of-comp-exp-lt expr1 list-of-expr env))
      (comp-expr-gt (expr1 list-of-expr) (value-of-comp-exp-gt expr1 list-of-expr env));started
      (exp-expr (expr1 list-of-expr) (value-of-exp-expr expr1 list-of-expr env)) ;started
      (if-then-else-expr (if-expr else-expr then-expr) (if-expr else-expr then-expr env)) ;started
      (let-expr (list-of-ID list-of-expr in-expr) (value-of-let-exp list-of-ID list-of-expr in-expr env)) ;started
      (letrec-expr (list-of-ID list-of-expr in-expr) (value-of-letrec-exp list-of-ID list-of-expr in-expr env)) ;started
      (proc-expr (list-of-ID expr) (value-of-proc-exp list-of-ID expr env))
      (set-expr (ID expr) (value-of-set-exp ID expr env))
      (begin-expr (begin-expr list-of-expr) (value-of-begin-exp))
      (newref-expr (expr) (value-of-newref-exp expr env))
      (else (report-invalid-syntax parsed-struct)))))
;(trace value-of-static)

;;;Handles ID expressions
;This comes from the mp2-3 solution
;(let ([value (apply-env env var)])
;    (if (eq? value 'not-found)
;        (variable var)
;        value))
(define value-of-id-exp
  (lambda (value env)
    (display "Apply the environment and deref store")
    ;And we need to check the store
    ;(deref (apply-env env value))
    ))
;(trace value-of-id-exp)

;;;Handles Arithmetic expressions
(define value-of-arith-exp
  (lambda (operator expr1 list-of-expr env)
    ;;;This one should look like Problem 2 from the last MP
    (value-of-arith-exp-rec* operator expr1 list-of-expr env)
    ))
;(trace value-of-arith-exp)

(define value-of-arith-exp-rec*
  (lambda (operator expr list-of-expr env)
    (if (null? list-of-expr)
       (value-of-static expr env)
       ((eval operator) (value-of-static expr env) (value-of-arith-exp-rec* operator (car list-of-expr)(cdr list-of-expr)env)))))
;(trace value-of-arith-exp-rec*)

;;;Handles Comparitive expressions
(define value-of-comp-exp-eq
  (lambda (expr1 expr2 env)
    ;;;This should also look like Problem 2 from the last MP
    (= (value-of-static expr1 env)(value-of-static expr2 env))
    ))
;(trace value-of-comp-exp)

(define value-of-comp-exp-lt
  (lambda (expr1 expr2 env)
    ;;;This should also look like Problem 2 from the last MP
    (< (value-of-static expr1 env)(value-of-static expr2 env))
    ))
;(trace value-of-comp-exp)

(define value-of-comp-exp-gt
  (lambda (expr1 expr2 env)
    ;;;This should also look like Problem 2 from the last MP
    (> (value-of-static expr1 env) (value-of-static expr2 env))
    ))
;(trace value-of-comp-exp)

;;;Handles multiple expressions
(define value-of-exp-expr 
  (lambda (expr1 list-of-expr env) 
    (display "do something about parenthesized expressions")
    ))
;(trace value-of-exp-expr)

;;;Handles if-then-else expressions
(define value-of-if-then-else-expr
  (lambda (if-expr else-expr then-expr env)
    (display "do something about if-then-else")
    ))
;(trace value-of-if-then-else-expr)

;;;Let expressions are handled in lecture code
;;;Handles Let expressions
;mp2-3 solution
;(define (value-of-let var expr1 expr2 env)
;  (value-of expr2 (extend-env env var (value-of expr1 env))))
;
;lecture let
;
(define value-of-let-exp
  (lambda (list-of-ID list-of-expr in-expr env)
    ;;;This should also look like Problem 2 from the last MP
    (display "do something about let")
    ))
;(trace value-of-let-exp)

;;;Handles Letrec expressions
(define value-of-letrec-exp
  (lambda (list-of-ID list-of-expr in-expr env)
    ;;;This should also look like Problem 2 from the last MP
    (display "do something about letrec")
    ))
;(trace value-of-letrec-exp)

;;;Procedures are handled in lecture code
;;;Handles Procedure expressions
(define value-of-proc-exp
  (lambda (list-of-ID expr env) 
    (display "do something about procedures")
  ))
;(trace value-of-proc-exp)

;;;Handles set case
(define value-of-set-exp
  (lambda (ID expr env)
    (display "do something about set")))
;(trace value-of-set-exp)

;;;Handles begin case
(define value-of-begin-exp
  (lambda (begin-expr list-of-expr env)
    (display "do something about begin")
    ))
;(trace value-of-begin-exp)

;;;Handles newref case
(define value-of-newref-exp
  (lambda (expr env)
    (display "do something about newref")))
;(trace value-of-newref-exp)