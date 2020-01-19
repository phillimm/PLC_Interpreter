; parse
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define (literal? data)
  (or (number? data) (symbol? data) (string? data)
   (boolean? data) (vector? data) (list? data)))

   ; An auxiliary procedure that could be helpful.
;(define var-exp?
;  (lambda (x)
;    (cases expression x
;      [var-exp (id) #t]
;      [else #f])))
;  (var-exp? (var-exp 'a))
;  (var-exp? (app-exp (var-exp 'a) (var-exp 'b)))

(define parse-exp
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(pair? datum)
        (cond
          [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
          [(eqv? (car datum) 'set!)
            (parse-set! datum)]
          [(eqv? (car datum) 'if)
            (parse-if datum)]
          [(eqv? (car datum) 'lambda)
            (parse-lambda datum)]
          [(or (eqv? (car datum) 'let) (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec))
            (parse-let datum)]
          [else (parse-app datum)])]
          [(literal? datum) (lit-exp datum)]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (parse-let datum)
  (let ([len (length datum)])
    (cond
      [(> 3 len)
        (eopl:error 'parse-exp "let expression: incorrect length: ~s" datum)]
      [(not (and (list? (cadr datum)) (andmap list? (cadr datum))))
        (eopl:error 'parse-exp "decls: not a proper list: ~s" datum)]
      [(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
        (eopl:error 'parse-exp "lambda: first members must be symbols")]
      [(not (andmap (lambda (x) (eq? 2 (length x))) (cadr datum)))
        (eopl:error 'parse-exp "decls: not all length 2: ~s" datum)]
      ; named let
      [(eq? (car datum) 'let*)
        (let*-exp (map car (cadr datum))
                       (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
                       (map parse-exp (cddr datum)))]
      [(eq? 'letrec (car datum))
        (letrec-exp (map car (cadr datum))
                       (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
                       (map parse-exp (cddr datum)))]
      [else (let-exp (map car (cadr datum))
                     (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
                     (map parse-exp (cddr datum)))])))

(define (parse-app datum)
  (cond
    [(not (list? datum))
      (eopl:error 'parse-exp "application: ~s is not a proper list" datum)]
    [else (app-exp (parse-exp (1st datum))
                   (map parse-exp (cdr datum)))]))

(define (parse-lambda datum)
    (cond
      [(null? (cddr datum))
        (eopl:error 'parse-exp "lambda expression: incorrect number of arguments ~s" datum)]
      [(null? (cadr datum))
        (lambda-exp (list ) (map parse-exp (cddr datum)))]
      [(symbol? (cadr datum))
        (lambda-nonfixed-exp (lit-exp (cadr datum)) (map parse-exp (cddr datum)))]
      [(list? (cadr datum))
        (if (andmap symbol? (cadr datum))
            (lambda-exp (map lit-exp (cadr datum)) (map parse-exp (cddr datum)))
            (eopl:error 'parse-exp "lambda expression: formals must be symbols ~s" datum)
        )]
      [(pair? (cadr datum))
        (lambda-opt-exp (map parse-exp (cadr datum))
                        (parse-exp (cdr (cadr datum)))
                        (map parse-exp (caddr datum)))]
      [else (eopl:error 'parse-exp "lambda expression not valid: ~s" datum)]))

(define (parse-if datum)
  (let ([len (length datum)])
    (cond
      [(or (null? (cdr datum)) (null? (cddr datum)))
        (eopl:error 'parse-exp "if expression: should have (only) test, then,
          and else clauses: ~s" datum)]
      [(null? (cdddr datum))
        (if-no-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))]
      [(null? (cddddr datum))
        (if-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))
                (parse-exp (cadddr datum)))]
      [else (eopl:error 'parse-exp "if expression: too many args ~s" datum)])))

  (define (unparse-exp datum)
      (cases expression datum
          [var-exp (var) var]
          [lit-exp (var) var]
          [lambda-exp (vars bodies)
                (cons 'lambda (cons vars
                                    (map unparse-exp bodies)))]
          [lambda-nonfixed-exp (var bodies)
            (cons 'lambda (cons var (map unparse-exp bodies)))]
          [lambda-opt-exp (vars opt bodies)
                      (cons 'lambda (cons (cons vars (unparse-exp opt))
                          (map unparse-exp bodies)))]
          [set!-exp (var exp)
            (list 'set! var (unparse-exp exp))]
          [let-exp (vars exps bodies)
            (cons 'let (cons (map (lambda (x y) (list x (unparse-exp y))) vars exps)
                        (map unparse-exp bodies)))]
          [letrec-exp (vars exps bodies)
            (cons 'letrec (cons (map (lambda (x y) (list x (unparse-exp y))) vars exps)
                        (map unparse-exp bodies)))]
          [let*-exp (vars exps bodies)
            (cons 'let* (cons (map (lambda (x y) (list x (unparse-exp y))) vars exps)
                  (map unparse-exp bodies)))]
          [if-else-exp (condition then else)
            (list 'if (unparse-exp condition)
                 (unparse-exp then)
                 (unparse-exp else)) ]
          [if-no-else-exps (condition then)
            (cons 'if (cons (unparse-exp condition)
                 (unparse-exp then)))]
          [app-exp (rator rands)
            (cons (unparse-exp rator) (map unparse-exp rands))]))
