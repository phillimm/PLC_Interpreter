(define (v-cons element source)
  (let ((newv (make-vector (+ 1 (vector-length source)))))
    (vector-set! newv 0 element)
    (vector-copy! newv 1 source 0)
    newv))

(define (vector-copy! dest dest-start source source-start)
  (letrec ((loop (lambda (dest-start source-start)
                  (if (not (or
                    (= dest-start (vector-length dest))
                      (= source-start (vector-length source))))
                    (begin (vector-set! dest dest-start (vector-ref source source-start))
                      (loop (+ 1 dest-start) (+ 1 source-start)))))))
          (loop dest-start source-start)))

(define (extend-env symbols vals env)
    (extended-env symbols vals env))

(define (list-find-position symbol los)
    (list-index (lambda (xsymbol) (eqv? symbol xsymbol)) los))

(define (list-index pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f)))))

(define (add-to-global-env symbol value)
  (cases environment global-env
    [extended-env (symbols vals env)
      (set! global-env (extended-env (cons symbol symbols) (v-cons (box value) vals) env))]
    [else (eopl:error 'add-to-global-env "invalid environment!!! panic!!!!")]))

(define (reset-global-env)
  (set! global-env init-env))

(define (apply-k k v)
  (cases continuation k
    [empty-k () v]
    [test-then-k (then env k) (if v (eval-exp then env k) (apply-k k (void)))]
    [test-else-k (then else env k) (if v (eval-exp then env k) (eval-exp else env k))]
    [app-exp-k (rands env k) (eval-rands rands env (app-k v k))]
    [rands-k (rands env k) (eval-rands rands env (cons-k v k))]
    [app-k (rator k) (apply-proc rator v k)]
    [map-proc-k (proc args k) (map-proc proc args (cons-k v k))]
    [cons-k (first k) (apply-k k (cons first v))]
    [begin-k (bodies env k) (eval-bodies bodies env k)]
    [set!-k (ref k) (apply-k k (set-box! ref v))]))

(define (apply-env-addr addr env)
  (unbox (apply-env-addr-ref addr env)))

(define (apply-env-addr-ref addr env)
  (cases expression addr
    [address (depth position)
      (apply-env-reference depth position env)]
    [else (eopl:error 'apply-env "invalid address ~s" addr)]))

(define (apply-env depth position env)
  (unbox (apply-env-reference depth position env)))

(define (apply-env-reference depth position env)
  (if (number? depth)
    (apply-env-reference-local env depth position
      (lambda (v) v)
      (lambda () (eopl:error 'eval-exp "invalid var ~s ~s" depth position)))
    (apply-env-reference-global position
      (lambda (v) v)
      (lambda () (eopl:error 'eval-exp "invalid var ~s" position)))))

(define (apply-env-reference-global symbol pass fail)
  (cases environment global-env
    [empty-env () (fail)]
    [extended-env (symbols vals env)
      (let ((pos (list-find-position symbol symbols)))
            (if (number? pos)
              (pass (vector-ref vals pos))
              (fail)))]))

(define (apply-env-reference-local env depth position pass fail)
  (cases environment env
    [empty-env () (fail)]
    [extended-env (symbols vals env)
      (if (= 0 depth)
        (vector-ref vals position)
        (apply-env-reference-local env (- depth 1) position pass fail))]))
