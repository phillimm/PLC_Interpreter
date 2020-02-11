;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment


(define (top-level-eval form)
    (cases expression form
      [define-exp (var body) (add-to-global-env var (eval-exp body (empty-env) (empty-k)))]
      [begin-exp (bodies) (for-each top-level-eval bodies)]
      [else (eval-exp form (empty-env) (empty-k))])
    ; later we may add things that are not expressions.
  )
; eval-exp is the main component of the interpreter

(define (eval-exp exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-k k datum)]
;      [var-exp (var)
;        (apply-env-with-global env id)]
      [address (depth position)
        (apply-k k (apply-env-addr exp env))]
      [app-exp (rator rands)
        (eval-exp rator env (app-exp-k rands env k))]
      [if-else-exp (condition then else)
        (eval-exp condition env (test-else-k then else env k))]
      [if-no-else-exp (condition then)
        (eval-exp condition env (test-then-k then env k))]
      [while-exp (test bodies)
        (if (eval-exp test env)
          (begin (eval-bodies bodies env k)
                  (eval-exp exp env k)))]
      [lambda-exp (vars bodies)
        (apply-k k (closure-standard vars bodies env))]
      [lambda-nonfixed-exp (var bodies)
        (apply-k k (closure-nonfixed var bodies env))]
      [lambda-opt-exp (vars opt bodies)
        (apply-k k (closure-opt vars opt bodies env))]
      [begin-exp (bodies)
        (eval-bodies bodies env k)]
      [set!-address-exp (id exp)
        (eval-exp exp env (set!-k (apply-env-addr-ref id env) k))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~s" exp)]))


; evaluate the list of operands, putting results into a list

(define (eval-rands rands env k)
  (cond [(null? rands) (apply-k k '())]
        [else (eval-exp (car rands) env (rands-k (cdr rands) env k))]))

(define eval-bodies
  (lambda (bodies env k)
    (cond [(null? (cdr bodies)) (eval-exp (car bodies) env k)]
          [else (eval-exp (car bodies) env (begin-k (cdr bodies) env k))])))



;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.

(define (take ls num)
  (if (= 0 num)
    '()
    (cons (car ls) (take (cdr ls) (- num 1)))))

(define (drop ls num)
  (if (or (= 0 num) (null? ls))
    ls
    (drop (cdr ls) (- num 1))))


    (define (eval-ref-params vars args env)
      (map (lambda (x y)
              (if (expression? y)
                  (cases expression y
                    [ref-exp (id)
                      (cases expression x
                        [address (depth position)
                          (if (number? depth)
                            (apply-env-ref env depth position
                              (lambda (v) v)
                              (lambda ()
                                (eopl:error 'eval-exp "invalid address ~s ~s" depth position)))
                            (apply-global-env-ref position
                              (lambda (v) v)
                                (lambda () (eopl:error 'eval-exp "invalid var in global ~s" position))))]
                        [else (box (eval-exp x env))])]
                    [else (eopl:error 'apply-proc "invalid exp ~s" y)])
                  (box (eval-exp x env)))) args vars))

    ;; Need to check for argument lengths for the stand and opt closures
(define (apply-proc proc-value args k)
  (cases proc-val proc-value
    [prim-proc (op) (apply-prim-proc op args k)]
    [closure-standard (vars bodies env)
      (eval-bodies bodies (extend-env vars (list->vector (map box args)) env) k)]
    [closure-nonfixed (var bodies env)
      (eval-bodies bodies (extend-env
                            (list var)
                            (list->vector (map box (cons args '())))
                            env) k)]
    [closure-opt (vars opt bodies env)
      (eval-bodies bodies (extend-env
                            (append vars (list opt))
                            (list->vector (map box (append (take args (length vars)) (list (drop args (length vars))))))
                            env) k)]
    [else (error 'apply-proc
                 "Attempt to apply bad procedure: ~s"
                  proc-value)]))

(define *prim-proc-names* '(+ - * / = > < <= >= add1 sub1 zero? cons car cdr list null? assq eq? eqv?
      equal? atom? length list->vector list? pair? procedure? vector->list vector
      make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set!
      display newline not map apply void quotient append list-tail))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (list->vector (map (lambda (x) (box (prim-proc x)))
          *prim-proc-names*))
     (empty-env)))

(define global-env init-env)

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.

(define (check-length proc num-args args k)
  (if (eq? num-args (length args))
    (apply-k k (apply proc args))
    (eopl:error prim-proc "improper number of argumments")))

(define (apply-prim-proc prim-proc args k)
  (let ((proc (symbol->string prim-proc)))
    (if (c...r? proc)
      ((make-c...r proc) (car args))

    (case prim-proc
      ; any number of arguments

      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(=) (apply-k k (apply = args))]
      [(<) (apply-k k (apply < args))]
      [(>) (apply-k k (apply > args))]
      [(<=) (apply-k k (apply <= args))]
      [(>=) (apply-k k (apply >= args))]
      [(append) (apply-k k (apply append args))]
      [(vector) (apply-k k (apply vector args))]
      [(list) (apply-k k args)]

      ; no arguments
      [(newline) (check-length newline 0 args k)]
      [(void) (check-length void 0 args k)]
  ;    [(reset-global-env)
  ;          (set! global-env init-env)]

      ; 1 argument
      [('quote) (check-length (lambda (x) x) 1 args k)]
      [(add1) (check-length add1 1 args k)]
      [(sub1) (check-length sub1 1 args k)]
      [(zero?) (check-length zero? 1 args k)]
      [(null?) (check-length null? 1 args k)]
      [(atom?) (check-length atom? 1 args k)]
      [(list->vector) (check-length list->vector 1 args k)]
      [(list?) (check-length list? 1 args k)]
      [(pair?) (check-length pair? 1 args k)]
      [(vector->list) (check-length vector->list 1 args k)]
      [(vector?) (check-length vector? 1 args k)]
      [(number?) (check-length number? 1 args k)]
      [(symbol?) (check-length symbol? 1 args k)]
      [(display) (check-length display 1 args k)]
      [(not) (check-length not 1 args k)]
      [(length) (check-length length 1 args k)]
      [(car) (check-length car 1 args k)]
      [(cdr) (check-length cdr 1 args k)]

      ; 2 arguments
      [(cons) (check-length cons 2 args k)]
      [(assq) (check-length assq 2 args k)]
      [(eq?) (check-length eq? 2 args k)]
      [(equal?) (check-length equal? 2 args k)]
      [(eqv?) (check-length eqv? 2 args k)]
      [(quotient) (check-length quotient 2 args k)]
      [(list-tail) (check-length list-tail 2 args k)]
      [(make-vector) (check-length make-vector 2 args k)]
      [(vector-ref) (check-length vector-ref 2 args k)]
      [(set-car!) (check-length set-car! 2 args k)]
      [(set-cdr!) (check-length set-cdr! 2 args k)]

      ; 3 arguments
      [(vector-set!) (check-length vector-set! 3 args k)]

      ; special prim-procs

      [(procedure?) (check-length (lambda (x) (or
                                                  (proc-type? x)
                                                  (and (symbol? x)
                                                    (c...r? (symbol->string x)))))
                                      1 args k)]
      [(apply) (if (= (length args) 2)
                (apply-proc (car args) (cadr args) k)
                (eopl:error prim-proc "improper number of argumments"))]
      [(map) (if (= (length args) 2)
              (map-proc (car args) (cadr args) k)
              (eopl:error prim-proc "improper number of argumments ~s" prim-proc))]
      [else (eopl:error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)]))))



(define (map-proc proc args k)
   (cond
     [(null? args) (apply-k k '())]
     [else (apply-proc proc (list (car args)) (map-proc-k proc (cdr args) k))]))

     (define rep      ; "read-eval-print" loop.
       (lambda ()
         (display "--> ")
         ;; notice that we don't save changes to the environment...
         (let ([answer (top-level-eval (lexical-address (syntax-expand (parse-exp (read)))))])
           ;; TODO: are there answers that should display differently?
           (eopl:pretty-print answer) (newline)
           (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (lexical-address (syntax-expand (parse-exp x))))))
