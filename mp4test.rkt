#lang eopl

;;;------------------------------------Errors
(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref "illegal reference ~s in store ~s" ref the-store)))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))

(define (report-invalid-syntax val)
  ((eopl:error 'parse-exp "Invalid syntax ~s" val)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
;;;------------------------------------/Errors

;;;------------------------------------Expressed Value
(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?))
  (proc-val 
    (proc proc?))
  (ref-val
    (ref reference?)))

;;Expressed Value Extractors/Utility
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval->ref
  (lambda (v)
    (cases expval v
      (ref-val (ref) ref)
      (else (expval-extractor-error 'reference v)))))
;;;------------------------------------/Expressed Value

;;;------------------------------------Store
(define the-store 'uninitialized)

(define empty-store
  (lambda () '()))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define get-store
  (lambda () the-store))

;;Part of the grammar
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
        (when (instrument-newref)
          (eopl:printf "newref: allocating location ~s with initial contents ~s~%" next-ref val))                     
        next-ref)))

;;Part of the grammar
(define deref 
  (lambda (ref)
    (list-ref the-store ref)))

;;Part of the grammar
(define setref!                       
  (lambda (ref val)
    (set! the-store
      (letrec
        ((setref-inner
           ;; returns a list like store1, except that position ref1
           ;; contains val. 
           (lambda (store1 ref1)
             (cond
               ((null? store1)(report-invalid-reference ref the-store))
               ((zero? ref1)(cons val (cdr store1)))
               (else
                 (cons
                   (car store1)
                   (setref-inner
                     (cdr store1) (- ref1 1))))))))
        (setref-inner the-store ref)))))

;;Store Extractors/Utility
;used to ensure it is a reference
(define reference?
  (lambda (v)
    (integer? v)))

;I don't understand this line?
(define instrument-newref (make-parameter #f))

;I don't think this is necessary
(define get-store-as-list
   (lambda ()
     (letrec
       ((inner-loop
          ;; convert sto to list as if its car was location n
          (lambda (sto n)
            (if (null? sto)
              '()
              (cons
                (list n (car sto))
                (inner-loop (cdr sto) (+ n 1)))))))
       (inner-loop the-store 0))))
;;;------------------------------------/Store

;;;------------------------------------Environment
;env as data-structure
(define-datatype environment environment?
  (empty-env)
  (extend-env 
    (bvar symbol?)
    (bval reference?)                 ; new for implicit-refs
    (saved-env environment?))
  (extend-env-rec*
    (proc-names (list-of symbol?))
    (b-vars (list-of symbol?))
    (proc-bodies (list-of expression?))
    (saved-env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
        (eopl:error 'apply-env "No binding for ~s" search-var))
      (extend-env (bvar bval saved-env)
        (if (eqv? search-var bvar)
	  bval
	  (apply-env saved-env search-var)))
      (extend-env-rec* (p-names b-vars p-bodies saved-env)
        (let ((n (location search-var p-names)))
          ;; n : (maybe int)
          (if n
            (newref
              (proc-val
                (procedure 
                  (list-ref b-vars n)
                  (list-ref p-bodies n)
                  env)))
            (apply-env saved-env search-var)))))))
;;Environment Extractors/Procedures
(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n) 
            (+ n 1)))
      (else #f))))

;;I don't think we'll need this as the book says
;;it is used for debugging
(define env->list
  (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym val)              ; val is a denoted value-- a reference. 
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

;;;------------------------------------/Environment

;;;------------------------------------Procedure
(define-datatype proc proc?
  (procedure
    (bvar symbol?)
    (body expression?)
    (env environment?)))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of-static body
          (extend-env var (newref val) saved-env))))))
;;;------------------------------------/Procedure

;;;SLLGEN
;;;Lexical configuration for the problem
(define lexical
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    (arith-ops ((or "+" "-" "*" "/")) symbol)
    ;(comp-ops ((or "<" ">" "=")) symbol)                 ;We may need to change this to be able to pull out the operator
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
;(trace scan&parse)
  
(define just-scan
  (sllgen:make-string-scanner lexical grammar))

;;;------------------------------------Evaluator
;;The definition of the interpreter
(define static-interpreter 
  (lambda (input)
    (initialize-store!)
    (value-of-static (scan&parse input) empty-env)))
;(trace static-interpreter)

;;;Handles the separate cases of input
(define value-of-static 
  (lambda (parsed-struct env) 
    (cases expression parsed-struct
      (true-expr (value) value)                                                                                           ;done
      (false-expr (value) value)                                                                                          ;done
      (undefined-expr (value) value)                                                                                      ;done
      (num-expr (value) value)                                                                                            ;done
      (id-expr (value) (value-of-id-exp value env))                                                                       ;started
      (arith-expr (operator expr1 list-of-expr) (value-of-arith-exp operator expr1 list-of-expr env))                     ;done
      ;(comp-expr (operator expr1 list-of-expr) (value-of-comp-exp operator expr1 list-of-expr env))
      (comp-expr-eq (expr1 list-of-expr) (value-of-comp-exp-eq expr1 list-of-expr env))                                   ;done
      (comp-expr-lt (expr1 list-of-expr) (value-of-comp-exp-lt expr1 list-of-expr env))                                   ;done
      (comp-expr-gt (expr1 list-of-expr) (value-of-comp-exp-gt expr1 list-of-expr env))                                   ;done
      (exp-expr (expr1 list-of-expr) (value-of-exp-expr expr1 list-of-expr env))                                          ;started -> Tail Recursion? Evaluate right to left
      (if-then-else-expr (comp-expr if-T-expr if-F-expr) (value-of-if-then-else-expr comp-expr if-T-expr if-F-expr env))  ;done
      (let-expr (list-of-ID list-of-expr in-expr) (value-of-let-exp list-of-ID list-of-expr in-expr env))                 ;started
      (letrec-expr (list-of-ID list-of-expr in-expr) (value-of-letrec-exp list-of-ID list-of-expr in-expr env))           ;started
      (proc-expr (list-of-ID expr) (value-of-proc-exp list-of-ID expr env))                                               ;
      (set-expr (ID expr) (value-of-set-exp ID expr env))                                                                 ;
      (begin-expr (begin-expr list-of-expr) (value-of-begin-exp))                                                         ;
      (newref-expr (expr) (value-of-newref-exp expr env))                                                                 ;
      (else (report-invalid-syntax parsed-struct)))))                                                                     ;else
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
  (lambda (comp-expr if-T-expr if-F-expr env)
    (if (value-of-static comp-expr env)
        (value-of-static if-T-expr env)
        (value-of-static if-F-expr env))
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
    (value-of-static in-expr (mult-extension list-of-ID list-of-expr env))
    ))
;(trace value-of-let-exp)

;;;Multiple Extension in Let
(define mult-extension
  (lambda (list-of-ID list-of-expr env)
    (if (null? list-of-ID)
        (env)
        (mult-extension 
         (cdr list-of-ID)
         (cdr list-of-expr) 
         (extend-env (car list-of-ID)(value-of-static (car list-of-expr))env)))))

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