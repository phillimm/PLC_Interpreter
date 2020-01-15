;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form)))

; eval-exp is the main component of the interpreter

(define (eval-exp exp)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (if (c..r? (symbol->string id))
          (prim-proc id)
  				(apply-env init-env id; look up its value.
        	   (lambda (x) x) ; procedure to call if it is in the environment
             (lambda () (eopl:error 'apply-env ; procedure to call if it is not in env
  		          "variable not found in environment: ~s"
  			   id))))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator)]
              [args (eval-rands rands)])
          (apply-proc proc-value args))]
      [if-else-exp (condition then else)
        (if (eval-exp condition)
            (eval-exp then)
            (eval-exp else))]
      [if-no-else-exp (condition then)
        (if (eval-exp condition)
            (eval-exp then)
            (void))]
      [lambda-exp (vars bodies)
        (closure-standard vars bodies)]
      [lambda-nonfixed-exp (var bodies)
        (closure-nonfixed var bodies)]
      [lambda-component (vars opt bodies)
        (closure-opt vars opt bodies)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands)
    (map eval-exp rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                    proc-value)])))

(define *prim-proc-names* '(+ - * / = > < <= >= add1 sub1 zero? cons car cdr list null? assq eq? eqv?
      equal? atom? length list->vector list? pair? procedure? vector->list vector
      make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set!
      display newline not map apply void quotient append list-tail exit-list call/cc))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.

(define (check-length prim-proc num-args args)
  (if (eq? num-args (length args))
    (apply prim-proc args)
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
