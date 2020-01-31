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
          [(eqv? (car datum) 'or)
            (parse-or datum)]
          [(eqv? (car datum) 'cond)
            (parse-cond datum)]
          [(eqv? (car datum) 'and)
            (parse-and datum)]
          [(eqv? (car datum) 'begin)
            (parse-begin datum)]
          [(eqv? (car datum) 'case)
            (parse-case datum)]
          [(eqv? (car datum) 'while)
            (parse-while datum)]
          [(or (eqv? (car datum) 'let) (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec))
              (parse-let datum)]
          [(eqv? (car datum) 'define)
            (parse-define datum)]
          [else (parse-app datum)])]
      [(literal? datum) (lit-exp datum)]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (syntax-expand exp)
  (cases expression exp
      [lambda-exp (vars bodies)
        (lambda-exp (map syntax-expand vars) (map syntax-expand bodies))]
      [lambda-nonfixed-exp (var bodies)
        (lambda-nonfixed-exp var (map syntax-expand bodies))]
      [lambda-opt-exp (vars opt bodies)
        (lambda-opt-exp (map syntax-expand vars) opt (map syntax-expand bodies))]
      [set!-exp (var exp)
        (set!-exp var (syntax-expand exp))]
      [let-exp (vars exps bodies)
        (app-exp (lambda-exp vars (map syntax-expand bodies))
                 (map syntax-expand exps))]
      [letrec-exp (vars exps bodies)
        (syntax-expand-letrec vars exps bodies)]
      [let*-exp (vars exps bodies)
        (syntax-expand-let* vars exps bodies)]
      [if-else-exp (condition then else)
        (if-else-exp (syntax-expand condition) (syntax-expand then) (syntax-expand else))]
      [if-no-else-exp (condition then)
        (if-no-else-exp (syntax-expand condition) (syntax-expand then))]
      [begin-exp (bodies)
      ;  (app-exp (lambda-exp (list ) (map syntax-expand bodies)) (list ))]
        (begin-exp (map syntax-expand bodies))]
      [or-exp (bodies)
        (syntax-expand-or bodies)]
      [cond-no-else-exp (conditions results)
        (syntax-expand-cond conditions '() results)]
      [cond-else-exp (conditions else results)
        (syntax-expand-cond conditions else results)]
      [and-exp (bodies)
        (syntax-expand-and bodies)]
      [case-no-else-exp (test conditions results)
        (syntax-expand-case test conditions '() results)]
      [case-else-exp (test conditions else results)
        (syntax-expand-case test conditions else results)]
      [while-exp (test bodies)
        (syntax-expand-while test bodies)]
      [define-exp (binding body)
        (define-exp binding (syntax-expand body))]
      [else exp]))


(define (parse-while datum)
  (while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum))))

(define (syntax-expand-while test bodies)
;  (syntax-expand-letrec
;    (list (lit-exp 'helper))
;    (list (lambda-exp '()
;            (list (if-no-else-exp test
;                                 (begin-exp (append (cdr bodies)
;                                             (list (app-exp (var-exp 'helper) '()))))))))
;    (list (app-exp (var-exp 'helper) '()))))
    (while-exp (syntax-expand test)
        (map syntax-expand bodies)))


(define (syntax-expand-letrec vars exps bodies)
  (syntax-expand
    (let-exp vars
      (make-list (length vars) (lit-exp #f))
      (append (map (lambda (x y)
                      (set!-exp x y))
                    vars exps) bodies))))
(define (syntax-expand-let* vars exps bodies)
  (cond [(null? vars)
           (app-exp
              (lambda-exp '() (map syntax-expand bodies))
              '())]
        [(= 1 (length vars))
          (app-exp (lambda-exp vars
                               (map syntax-expand bodies))
                               (map syntax-expand exps))]
        [else (app-exp (lambda-exp (list (car vars))
                                   (list (syntax-expand-let*
                                                (cdr vars)
                                                (cdr exps)
                                                bodies)))
                        (list (syntax-expand (car exps))))]))

(define (syntax-expand-and bodies)
  (cond [(null? bodies)
            (lit-exp #t)]
        [(null? (cdr bodies))
          (if-no-else-exp (syntax-expand (car bodies))
                       (lit-exp #t))]
        [else
            (if-else-exp (syntax-expand (car bodies))
                         (syntax-expand-and (cdr bodies))
                         (lit-exp #f))]))

(define (syntax-expand-cond conditions else results)
  (cond [(null? conditions)
            (if (null? else)
                (app-exp (var-exp 'void) '())
                (syntax-expand (begin-exp else)))]
        [else (if-else-exp (syntax-expand (car conditions))
                           (syntax-expand (car results))
                           (syntax-expand-cond (cdr conditions)
                                               else
                                               (cdr results)))]))

(define (syntax-expand-or bodies)
  (cond [(null? bodies) (lit-exp #f)]
        [else (let-exp (list (lit-exp 'test))
                       (list (syntax-expand (car bodies)))
                       (list (if-else-exp (var-exp 'test)
                                          (var-exp 'test)
                                          (syntax-expand-or (cdr bodies)))))]))

(define (parse-define datum)
  (define-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))))

(define (parse-or datum)
  (or-exp (map parse-exp (cdr datum))))

(define (parse-and datum)
  (and-exp (map parse-exp (cdr datum))))

(define (parse-begin datum)
  (begin-exp (map parse-exp (cdr datum))))

(define (parse-cond-conditions results datum)
  (cond [(null? datum)
            (list (reverse results))]
        [(eqv? (caar datum) 'else)
          (cons (reverse results) (list (cdar datum)))]
        [else (parse-cond-conditions (cons (car datum) results) (cdr datum))]))

(define (parse-cond datum)
  (let ((result (parse-cond-conditions '() (cdr datum))))
    (if (null? (cdr result))
        (cond-no-else-exp
            (map (lambda (x) (parse-exp (car x))) (car result))
            (map (lambda (x) (begin-exp (map parse-exp (cdr x)))) (car result)))
        (cond-else-exp
            (map (lambda (x) (parse-exp (car x))) (car result))
            (map parse-exp (cadr result))
            (map (lambda (x) (begin-exp (map parse-exp (cdr x)))) (car result))))))

(define (parse-case-conditions result datum)
  (cond [(null? datum)
          (list (reverse result))]
        [(eqv? (caar datum) 'else)
          (cons (reverse result)
                (list (cdar datum)))]
        [else (parse-case-conditions
                (cons (car datum) result)
                (cdr datum))]))

(define (parse-case datum)
  (let ([result (parse-case-conditions '() (cddr datum))])
    (if (null? (cdr result))
         (case-no-else-exp
           (parse-exp (cadr datum))
                   (map (lambda (x)
                          (syntax-expand-or
                            (map (lambda (y)
                                    (app-exp (var-exp 'eqv?)
                                             (list (lit-exp y)
                                                (parse-exp (cadr datum))))) (car x))))
                          (car result))
                      (map (lambda (x) (begin-exp (map parse-exp (cdr x))))
                                              (car result)))
          (case-else-exp (parse-exp (cadr datum))
              (map (lambda (x)
                      (syntax-expand-or
                        (map (lambda (y)
                                (app-exp
                                  (var-exp 'eqv?)
                                    (list (lit-exp y)
                                      (parse-exp (cadr datum))))) (car x))))
                    (car result))
              (map parse-exp (cadr result))
              (map (lambda (x)
                    (begin-exp (map parse-exp (cdr x)))) (car result)))
          )))

(define (let*->let ls)
        (letrec ((helper (lambda (lst)
            (cond [(null? lst) (caddr ls)]
                  [(symbol? lst) (list lst)]
                  [else (list 'let (list (car lst)) (helper (cdr lst)))  ]
            )
        ))) (helper (cadr ls))))

(define (syntax-expand-case-test conditions else results)
  (cond [(null? conditions)
          (if (null? else)
              (app-exp (var-exp 'void) '())
              (syntax-expand (begin-exp else)))]
        [(and (null? (cdr conditions)) (null? else))
          (if-no-else-exp (car conditions)
                          (car results))]
        [else
          (if-else-exp (car conditions)
                       (car results)
                       (syntax-expand-case-test (cdr conditions)
                                                else
                                                (cdr results)))]))



(define (syntax-expand-case test conditions else results)
  (app-exp
    (lambda-exp (list (lit-exp 'test))
                (list (syntax-expand
                          (syntax-expand-case-test conditions else results))))
          (list (syntax-expand test))))

;(define (parse-set! datum)
;    (cond [(or (null? (cdr datum)) (null? (cddr datum)) (not (null? (cdddr datum))))
;            (eopl:error 'parse-exp "set!: improper number of arguments: ~s" datum)]
;          [(not (symbol? (cadr datum)))
;                  (eopl:error 'parse-exp "set!: var to set is not a symbol ~s")]
;          [(not (null? (cdddr datum)))
;              (eopl:error 'parse-exp "set!: Too many arguments: ~s" datum)]
;          [else (set!-exp (lit-exp (cadr datum)) (parse-exp (caddr datum)))]))

          (define (parse-set! datum)
            (cond
              [(or (null? (cdr datum)) (null? (cddr datum)) (not (null? (cdddr datum))))
                (eopl:error 'parse-exp "set!-expression: improper number of arguments ~s" datum)]
              [(not (symbol? (cadr datum))) (eopl:error 'parse-exp "set!-expression: id is not a symbol ~s" datum)]
              [(null? (cdddr datum)) (set!-exp (lit-exp (cadr datum)) (parse-exp (caddr datum)))]
              [else (eopl:error 'parse-exp "set!-expression: too many arguments ~s" datum)]))


(define (parse-let datum)
  (let ([len (length datum)])
    (cond
      [(> 3 len)
        (eopl:error 'parse-exp "let expression: incorrect length: ~s" datum)]
          ; named let
          [(symbol? (cadr datum))
            (parse-named-let datum)]
      [(not (and (list? (cadr datum)) (andmap list? (cadr datum))))
        (eopl:error 'parse-exp "decls: not a proper list: ~s" datum)]
      [(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
        (eopl:error 'parse-exp "lambda: first members must be symbols")]
      [(not (andmap (lambda (x) (eq? 2 (length x))) (cadr datum)))
        (eopl:error 'parse-exp "decls: not all length 2: ~s" datum)]

      ;let*
      [(eqv? (car datum) 'let*)
        (let*-exp (map (lambda (x) (lit-exp (car x))) (cadr datum))
                     (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
                     (map parse-exp (cddr datum)))]
      [(eqv? 'letrec (car datum))
        (letrec-exp (map (lambda (x) (lit-exp (car x))) (cadr datum))
                       (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
                       (map parse-exp (cddr datum)))]
      [else (let-exp (map (lambda (x) (lit-exp (car x))) (cadr datum))
                     (map (lambda (x) (parse-exp (cadr x))) (cadr datum))
                     (map parse-exp (cddr datum)))])))

(define (parse-named-let datum)
  (letrec-exp   (list (lit-exp (cadr datum)))
              (list (lambda-exp
                (map (lambda (x) (lit-exp (car x))) (caddr datum))
                (map parse-exp (cdddr datum))))
              (list (app-exp (var-exp (cadr datum))
                       (map (lambda (x) (parse-exp (cadr x))) (caddr datum))))))

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

      (define (parse-vars v)
        (cond
          [(null? v) '()]
          [(not (symbol? (car v)))
            (eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" v)]
          [(symbol? (cdr v)) (list (lit-exp (car v)) (cdr v))]
          [else (cons (lit-exp (car v)) (parse-vars (cdr v)))]))

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
        (let ((l (reverse (parse-vars (cadr datum)))))
                          (lambda-opt-exp (reverse (cdr l))
                            (lit-exp (car l))
                            (map parse-exp (cddr datum))))]
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
