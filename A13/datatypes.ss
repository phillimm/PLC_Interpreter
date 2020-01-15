(define-datatype expression expression?
  [var-exp
   (var symbol?)]
  [lambda-exp
    (vars (list-of symbol?))
    (bodies (list-of expression?))]
  [lambda-nonfixed-exp
    (var symbol?)
    (bodies (list-of expression?))]
  [lambda-opt-exp
    (vars (list-of symbol?))
    (opt symbol?)
    (bodies (list-of expression?))]
  [lit-exp
    (var literal?)]
  [set!-exp
    (var symbol?)
    (exp expression?)]
  [let-exp
    (vars (list-of symbol?))
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [letrec-exp
    (vars (list-of symbol?))
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [let*-exp
    (vars (list-of symbol?))
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [if-else-exp
    (condition expression?)
    (then expression?)
    (else expression?)]
  [if-no-else-exps
    (condition expression?)
    (then expression?)]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))])



(define-datatype proc-type proc-type?
  [prim-proc
    (proc-name symbol?)]
  [closure-standard
    (var lit-exp?)
    (bodies (list-of expression?))
    (env environment?)]
  [closure-nonfixed
    (vars (list-of lit-exp?))
    (bodies (list-of expression?))
    (env environment?)]
  [closure-opt
    (vars (list-of lit-exp?))
    (opt-var lit-exp?)
    (bodies (list-of expression?))
    (env environment?)])

(define scheme-value?
  (lambda (x) #t))

(define (var-exp? x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))

(define (lit-exp? x)
  (cases expression x
    [lit-exp (id) '#t]
    [else '#f]))


(define (app-exp? x)
  (cases expression x
    [app-exp (rator rands) '#t]
    [else '#f]))

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define (literal? data)
  (or (number? data) (symbol? data) (string? data)
   (boolean? data) (vector? data)))
