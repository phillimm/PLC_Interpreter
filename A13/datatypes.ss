(define-datatype expression expression?
  [var-exp
   (var symbol?)]
  [ref-exp
    (id symbol?)]
  [lambda-exp
    (vars (list-of sym-ref?))
    (bodies (list-of expression?))]
  [lambda-nonfixed-exp
    (var sym-ref?)
    (bodies (list-of expression?))]
  [lambda-opt-exp
    (vars (list-of sym-ref?))
    (opt symbol?)
    (bodies (list-of expression?))]
  [lit-exp
    (var literal?)]
  [set!-exp
    (var symbol?)
    (exp expression?)]
  [set!-address-exp
    (var address?)
    (body expression?)]
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
  [if-no-else-exp
    (condition expression?)
    (then expression?)]
  [begin-exp
    (bodies (list-of expression?))]
  [or-exp
    (bodies (list-of expression?))]
  [and-exp
    (bodies (list-of expression?))]
  [cond-no-else-exp
    (conditions (list-of expression?))
    (results (list-of expression?))]
  [cond-else-exp
    (conditions (list-of expression?))
    (else (list-of expression?))
    (results (list-of expression?))]
  [case-no-else-exp
    (test expression?)
    (conditions (list-of expression?))
    (results (list-of expression?))]
  [case-else-exp
    (test expression?)
    (conditions (list-of expression?))
    (else (list-of expression?))
    (results (list-of expression?))]
  [while-exp
    (test expression?)
    (bodies (list-of expression?))]
  [define-exp
    (binding literal?)
    (body expression?)]
  [address
    (depth
      (lambda (x)
        (or (number? x)
            (eqv? x 'free))))
    (position
      (lambda (x)
        (or (number? x)
            (symbol? x))))]
  ;[ref-exp
  ;  (id symbol?)]
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
    (var (list-of sym-ref?))
    (bodies (list-of expression?))
    (env environment?)]
  [closure-nonfixed
    (var (list-of sym-ref?))
    (bodies (list-of expression?))
    (env environment?)]
  [closure-opt
    (vars (list-of sym-ref?))
    (opt-var symbol?)
    (bodies (list-of expression?))
    (env environment?)])

(define-datatype continuation continuation?
    [empty-k]
    [test-then-k
      (then expression?)
      (env environment?)
      (k continuation?)]
    [test-else-k
      (then expression?)
      (else expression?)
      (env environment?)
      (k continuation)]
    [app-exp-k
      (rands (list-of expression?))
      (env environment?)
      (k continuation?)]
    [app-k
      (rands proc-type?)
      (k continuation?)]
    [map-proc-k
      (proc proc-type?)
      (args (list-of scheme-value?))
      (k continuation?)]
    [cons-k
      (first scheme-value?)
      (k continuation?)]
    [begin-k
      (bodies (list-of expression?))
      (env environment?)
      (k continuation?)]
    [set!-k
      (ref box?)
      (k continuation?)]
    [rands-k
      (rands (list-of expression?))
      (env environment?)
      (k continuation?)])

(define (sym-ref? x)
  (or (symbol? x) (ref-exp? x)))

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

(define (ref-exp? x)
  (cases expression x
    [ref-exp (id) '#t]
    [else '#f]))


(define (app-exp? x)
  (cases expression x
    [app-exp (rator rands) '#t]
    [else '#f]))

(define (address? x)
  (cases expression x
    [address (depth position) '#t]
    [else '#f]))
