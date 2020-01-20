(define-datatype expression expression?
  [var-exp
   (var symbol?)]
  [lambda-exp
    (vars (list-of expression?))
    (bodies (list-of expression?))]
  [lambda-nonfixed-exp
    (var lit-exp?)
    (bodies (list-of expression?))]
  [lambda-opt-exp
    (vars (list-of expression?))
    (opt symbol?)
    (bodies (list-of expression?))]
  [lit-exp
    (var literal?)]
  [set!-exp
    (var lit-exp?)
    (exp expression?)]
  [let-exp
    (vars (list-of expression?))
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [letrec-exp
    (vars (list-of expression?))
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [let*-exp
    (vars (list-of expression?))
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [if-else-exp
    (condition expression?)
    (then expression?)
    (else expression?)]
  [if-no-else-exp
    (condition expression?)
    (then expression?)]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))])

(define-datatype environment environment?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals vector?)
    (env environment?)])


(define-datatype proc-val proc-type?
  [prim-proc
    (proc-name symbol?)]
  [closure-standard
    (var (list-of expression?))
    (bodies (list-of expression?))
    (env environment?)]
  [closure-nonfixed
    (var lit-exp?)
    (bodies (list-of expression?))
    (env environment?)]
  [closure-opt
    (vars (list-of expression?))
    (opt-var lit-exp?)
    (bodies (list-of expression?))
    (env environment?)])

(define scheme-value?
  (lambda (x) #t))

(define (var-exp? x)
   (cases expression x
     [var-exp (id) #t]
     [else #f]))

(define (lit-exp? x)
  (cases expression x
    [lit-exp (id) '#t]
    [else '#f]))


(define (app-exp? x)
  (cases expression x
    [app-exp (rator rands) '#t]
    [else '#f]))
