;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define (eval-exp exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env
            id
            (lambda (x) x)
            (lambda ()
              (if (c...r? (symbol->string id))
                (prim-proc id)
                (apply-env global-env id; look up its value.
                   (lambda (x) x) ; procedure to call if it is in the environment
                   (lambda () (eopl:error 'apply-env ; procedure to call if it is not in env
                      "variable not found in environment: ~s"
                 id))))))]
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
      [let-exp (vars exps bodies)
        (eval-bodies bodies
            (extend-env (map (lambda (x) (eval-exp x env)) vars)
              (list-> vector (map (lambda (x) (eval-exp x env)) exps)) env))]
      [letrec-exp (vars exps bodies)
        (eval-bodies bodies
            (extend-env (map (lambda (x) (eval-exp x env)) vars)
              (list-> vector (map (lambda (x) (eval-exp x env)) exps)) env))]
      [lambda-exp (vars bodies)
        (closure-standard vars bodies env)]
      [lambda-nonfixed-exp (var bodies)
        (closure-nonfixed var bodies env)]
      [lambda-opt-exp (vars opt bodies)
        (closure-opt vars opt bodies env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))

; evaluate the list of operands, putting results into a list

(define (eval-rands rands)
    (map eval-exp rands))

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
          (eval-bodies bodies (extend-env (eval-rands vars env) (list->vector args) env))]
        [closure-nonfixed (var bodies env)
          (eval-bodies bodies (extend-env
                                (eval-rands (list var) env)
                                (list->vector (cons args '()))
                                env))]
        [closure-opt (vars opt bodies env)
          (eval-bodies bodies (extend-env
                                (eval-rands (append vars (list opt)) env)
                                (list->vector (append (take args vars-len) (list (drop args vars-len))))
                                env))]
        [else (error 'apply-proc
                     "Attempt to apply bad procedure: ~s"
                      proc-value)])))

(define *prim-proc-names* '(+ - * / = > < <= >= add1 sub1 zero? cons car cdr list null? assq eq? eqv?
      equal? atom? length list->vector list? pair? procedure? vector->list vector
      make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set!
      display newline not map apply void quotient append list-tail exit-list call/cc))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (list->vector (map prim-proc
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
  (case prim-proc
    ; any number of arguments
    [(+) (apply prim-proc args)]
    [(-) (apply prim-proc args)]
    [(*) (apply prim-proc args)]
    [(/) (apply prim-proc args)]
    [(=) (apply prim-proc args)]
    [(<) (apply prim-proc args)]
    [(>) (apply prim-proc args)]
    [(<=) (apply prim-proc args)]
    [(>=) (apply prim-proc args)]
    [(append) (apply prim-proc args)]
    [(vector) (apply prim-proc args)]

    ; no arguments
    [(newline) (check-length prim-proc 0 args)]
    [(void) (check-length prim-proc 0 args)]

    ; 1 argument

    [(add1) (check-length prim-proc 1 args)]
    [(sub1) (check-length prim-proc 1 args)]
    [(zero?) (check-length prim-proc 1 args)]
    [(null?) (check-length prim-proc 1 args)]
    [(atom?) (check-length prim-proc 1 args)]
    [(list->vector) (check-length prim-proc 1 args)]
    [(list?) (check-length prim-proc 1 args)]
    [(pair?) (check-length prim-proc 1 args)]
    [(vector->list) (check-length prim-proc 1 args)]
    [(vector?) (check-length prim-proc 1 args)]
    [(number?) (check-length prim-proc 1 args)]
    [(symbol?) (check-length prim-proc 1 args)]
    [(display) (check-length prim-proc 1 args)]
    [(not) (check-length prim-proc 1 args)]
    [(length) (check-length prim-proc 1 args)]
    [(car) (check-length prim-proc 1 args)]
    [(cdr) (check-length prim-proc 1 args)]

    ; 2 arguments
    [(cons) (check-length prim-proc 2 args)]
    [(assq) (check-length prim-proc 2 args)]
    [(eq?) (check-length prim-proc 2 args)]
    [(equal?) (check-length prim-proc 2 args)]
    [(eqv?) (check-length prim-proc 2 args)]
    [(quotient) (check-length prim-proc 2 args)]
    [(list-tail) (check-length prim-proc 2 args)]

    [(make-vector) (check-length prim-proc 2 args)]
    [(vector-ref) (check-length prim-proc 2 args)]
    [(set-car!) (check-length prim-proc 2 args)]
    [(set-cdr!) (check-length prim-proc 2 args)]

    ; 3 arguments
    [(vector-set!) (check-length prim-proc 3 args)]

    ; special prim-procs

    [(procedure?) (check-length (lambda (x) (proc-val? x)) 1 args k)]
    [(apply) (if (= (length args) 2)
              (apply-proc (car args) (cadr args) k)
              (eopl:error prim-proc "improper number of argumments"))]
    [(map) (if (= (length args) 2)
            (map-proc (car args) (cadr args) k)
            (eopl:error prim-proc "improper number of argumments"))]
    [else (eopl:error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)]))

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

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))
