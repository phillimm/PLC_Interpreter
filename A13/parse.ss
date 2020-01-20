; parse
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define (literal? data)
  (or (number? data) (symbol? data) (string? data)
   (boolean? data) (vector? data) (list? data)))

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
          [(or (eqv? (car datum) 'or) (eqv? (car datum) 'cond) (eqv? (car datum) 'and)
            (eqv? (car datum) 'begin))
            (syntax-expand datum)]
          [(or (eqv? (car datum) 'let) (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec))
              (parse-let datum)]
          [else (parse-app datum)])]
          [(literal? datum) (lit-exp datum)]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

      (define (let*->let ls)
        (letrec ((helper (lambda (lst)
            (cond [(null? lst) (caddr ls)]
                  [(symbol? lst) (list lst)]
                  [else (list 'let (list (car lst)) (helper (cdr lst)))  ]
            )
        ))) (helper (cadr ls))))

(define (syntax-expand datum)
  (cond [(eqv? (car datum) 'let*)
            (parse-exp (let*->let datum))]
        [(eqv? (car datum) 'begin)
            (app-exp (lambda-exp (list ) (map parse-exp (cdr datum))) (list ))]
        [(eqv? (car datum) 'or)
          (syntax-expand-or (cdr datum))]
        [(eqv? (car datum) 'and)
          (syntax-expand-and (cdr datum))]
        [(eqv? (car datum) 'cond)
          (syntax-expand-cond (cdr datum))]))

(define (syntax-expand-cond datum)
  (cond [(eqv? (caar datum) 'else)
          (parse-exp (cadar datum))]
        [(null? (cdr datum))
          (if-no-else-exp (parse-exp (caar datum))
                           (parse-exp (cadar datum)))]
        [else (if-else-exp (parse-exp (caar datum))
                           (parse-exp (cadar datum))
                           (syntax-expand-cond (cdr datum)))]))


(define (syntax-expand-or datum)
  (cond [(null? datum) (lit-exp #f)]
        [else (let-exp (list (lit-exp 'test))
                    (list (parse-exp (car datum)))
                   (list (if-else-exp (var-exp 'test)
                                      (var-exp 'test)
                                      (syntax-expand-or (cdr datum)))))]))
(define (syntax-expand-and datum)
  (cond [(null? datum)
          (lit-exp #t)]
        [(null? (cdr datum))
          (if-else-exp (not (syntax-expand-and datum))
                        (lit-exp #f)
                        (syntax-expand-and (cdr datum)))]))

(define (parse-set! datum)
    (cond [(or (null? (cdr datum)) (null? (cddr datum)) (not (null? (cdddr datum))))
            (eopl:error 'parse-exp "set!: improper number of arguments: ~s" datum)]
          [(not (symbol? (cadr datum)))
                  (eopl:error 'parse-exp "set!: var to set is not a symbol ~s")]
          [(not (null? (cdddr datum)))
              (eopl:error 'parse-exp "set!: Too many arguments: ~s" datum)]
          [else (set!-exp (lit-exp (cadr datum)) (parse-exp (caddr datum)))]))


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
      ;  (let*-exp (map (lambda (x) (lit-exp (car x))) (cadr datum))
      ;                 (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
      ;                 (map parse-exp (cddr datum)))]
        (syntax-expand datum)]
      [(eq? 'letrec (car datum))
        (letrec-exp (map (lambda (x) (lit-exp (car x))) (cadr datum))
                       (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
                       (map parse-exp (cddr datum)))]
      [else (let-exp (map (lambda (x) (lit-exp (car x))) (cadr datum))
                     (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
                     (map parse-exp (cddr datum)))])))

(define (parse-app datum)
  (cond
    [(not (list? datum))
      (eopl:error 'parse-exp "application: ~s is not a proper list" datum)]
    [else (app-exp (parse-exp (1st datum))
                   (map parse-exp (cdr datum)))]))

(define (opt-args datum)
  (if (not (pair? datum))
      (list )
      (cons (car datum) (opt-args (cdr datum)))))

(define (get-opt datum)
  (if (not (pair? datum))
      datum
      (opt-args (cdr datum))))

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
          (lambda-opt-exp (map lit-exp (opt-args (cadr datum)))
                        (lit-exp (get-opt (cadr datum)))
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
