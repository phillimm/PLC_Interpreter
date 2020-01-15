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

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define (literal? data)
  (or (number? data) (symbol? data) (string? data)
   (boolean? data) (vector? data)))
