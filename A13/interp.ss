;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define (top-level-eval form)
  (cases expression form
      [define-exp (var body) (add-to-global-env var (eval-exp body (empty-env)))]
      [begin-exp (bodies) (for-each top-level-eval bodies)]
      [else (eval-exp form (empty-env))]))
    ; later we may add things that are not expressions.	    ; later we may add things that are not expressions.

; eval-exp is the main component of the interpreter

(define (eval-exp exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env-with-global env id)]
      [address (depth position)
        (if (number? depth)
          (apply-env env depth position
            (lambda (v) v)
            (eopl: error 'eval-exp "invalid var ~s ~s" depth position)))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [if-else-exp (condition then else)
        (if (eval-exp condition env)
            (eval-exp then env)
            (eval-exp else env))]
      [if-no-else-exp (condition then)
        (if (eval-exp condition env)
            (eval-exp then env)
            (void))]
  ;    [let-exp (vars exps bodies)
  ;      (eval-bodies bodies
  ;          (extend-env (map (lambda (x) (eval-exp x env)) vars)
  ;            (list->vector (map (lambda (x) (eval-exp x env)) exps)) env))]
      [set!-exp (var exp)
          (let ((id (eval-exp var env)))
              (set-box!
                (apply-env-ref-with-global env id)
                (eval-exp exp env)))]
      [while-exp (test bodies)
        (if (eval-exp test env)
          (begin (eval-bodies bodies env)
                  (eval-exp exp env)))]
  ;   [begin-exp (bodies)
  ;      (for-each top-level-eval bodies)]
      [lambda-exp (vars bodies)
        (closure-standard vars bodies env)]
      [lambda-nonfixed-exp (var bodies)
        (closure-nonfixed var bodies env)]
      [lambda-opt-exp (vars opt bodies)
        (closure-opt vars opt bodies env)]
      [begin-exp (bodies)
        (eval-bodies bodies env)]
      [set!-address-exp (var exp)
        (cases expression var
          [address (depth position)
            (set-box!
              (if (number? depth)
                (apply-env-ref env depth position
                  (lambda (v) v)
                  (lambda ()
                    (eopl:error 'eval-exp "invalid address ~s ~s" depth position)))
                (apply-env-ref env depth position
                  (lambda (v) v)
                  (lambda ()
                    (eopl: error 'eval-exp "invalid variable in global ~s" position))))
                 (eval-exp exp env))]
            [else (eopl:error 'eval-exp "invalid address ~s ~s" depth position)])]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~s" exp)]))


; evaluate the list of operands, putting results into a list

(define (eval-rands rands env)
    (map (lambda (e)
            (eval-exp e env)) rands))

(define (eval-bodies bodies env)
  (cond [(null? (cdr bodies))
            (eval-exp (car bodies) env)]
        [else (begin
                (eval-exp (car bodies) env)
                (eval-bodies (cdr bodies) env))]))



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

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
        [prim-proc (op) (apply-prim-proc op args)]
        [closure-standard (vars bodies env)
          (eval-bodies bodies
            (extend-env vars (list->vector (map box args)) env))]
        [closure-nonfixed (var bodies env)
          (eval-bodies bodies (extend-env
                                (list var)
                                (list->vector (map box (cons args '())))
                                env))]
        [closure-opt (vars opt bodies env)
          (eval-bodies bodies (extend-env
                                      (append vars (list opt))
                                      (list->vector (map box (append (take args vars-len) (list (drop args vars-len)))))
                                      env))]
        [else (error 'apply-proc
                     "Attempt to apply bad procedure: ~s"
                      proc-value)])))

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

(define (check-length proc num-args args)
  (if (eq? num-args (length args))
    (apply proc args)
    (eopl:error prim-proc "improper number of argumments")))

(define (apply-prim-proc prim-proc args)
  (let ((proc (symbol->string prim-proc)))
    (if (c...r? proc)
      ((make-c...r proc) (car args))

    (case prim-proc
      ; any number of arguments

      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(append) (apply append args)]
      [(vector) (apply vector args)]
      [(list) args]

      ; no arguments
      [(newline) (check-length newline 0 args)]
      [(void) (check-length void 0 args)]
      [(reset-global-env)
        (check-length
          (lambda ()
            (set! global-env init-env)) 0 args)]

      ; 1 argument
      [('quote) (check-length (lambda (x) x) 1 args)]
      [(add1) (check-length add1 1 args)]
      [(sub1) (check-length sub1 1 args)]
      [(zero?) (check-length zero? 1 args)]
      [(null?) (check-length null? 1 args)]
      [(atom?) (check-length atom? 1 args)]
      [(list->vector) (check-length list->vector 1 args)]
      [(list?) (check-length list? 1 args)]
      [(pair?) (check-length pair? 1 args)]
      [(vector->list) (check-length vector->list 1 args)]
      [(vector?) (check-length vector? 1 args)]
      [(number?) (check-length number? 1 args)]
      [(symbol?) (check-length symbol? 1 args)]
      [(display) (check-length display 1 args)]
      [(not) (check-length not 1 args)]
      [(length) (check-length length 1 args)]
      [(car) (check-length car 1 args)]
      [(cdr) (check-length cdr 1 args)]

      ; 2 arguments
      [(cons) (check-length cons 2 args)]
      [(assq) (check-length assq 2 args)]
      [(eq?) (check-length eq? 2 args)]
      [(equal?) (check-length equal? 2 args)]
      [(eqv?) (check-length eqv? 2 args)]
      [(quotient) (check-length quotient 2 args)]
      [(list-tail) (check-length list-tail 2 args)]
      [(make-vector) (check-length make-vector 2 args)]
      [(vector-ref) (check-length vector-ref 2 args)]
      [(set-car!) (check-length set-car! 2 args)]
      [(set-cdr!) (check-length set-cdr! 2 args)]

      ; 3 arguments
      [(vector-set!) (check-length vector-set! 3 args)]

      ; special prim-procs

      [(procedure?) (check-length (lambda (x) (or
                                                  (proc-type? x)
                                                  (and (symbol? x)
                                                    (c...r? (symbol->string x)))))
                                      1 args)]
      [(apply) (if (= (length args) 2)
                (apply-proc (car args) (cadr args))
                (eopl:error prim-proc "improper number of argumments"))]
      [(map) (if (= (length args) 2)
              (map-proc (car args) (cadr args))
              (eopl:error prim-proc "improper number of argumments"))]
      [else (eopl:error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)]))))

(define (c...r? str)
  (let ((len (string-length str)))
    (and (eqv? #\c (string-ref str 0))
      (eqv? #\r (string-ref str (- len 1)))
        (andmap (lambda (x) (or (eqv? #\d x) (eqv? #\a x)))
          (string->list (substring str 1 (- len 1)))))))

(define compose
   (case-lambda
       [() (lambda (x) x)]
       [(first . rest)
       (let ([composed-rest (apply compose rest)])
         (lambda (x) (first (composed-rest x))))]))

(define (make-c...r str)
   (let ((len (string-length str)))
       (apply compose (map (lambda (x)
                              (if (equal? x #\a) car cdr))
                           (string->list (substring str 1 (- len 1)))))))

(define (map-proc proc args)
   (cond
     [(null? args) '()]
     [else (cons (apply-proc proc (list (car args))) (map-proc proc (cdr args)))]))

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
