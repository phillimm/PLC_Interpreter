;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.
; Based on EoPL sections 2.2 and 2.3

(define extend-env
  (lambda (syms vals env)
    (extended-env syms vals env)))

(define (reset-global-env)
    (set! global-env init-env))

    (define (vector-cons elem src)
      (let ((newv (make-vector (+ 1 (vector-length src)))))
        (vector-set! newv 0 elem)
        (vector-copy! newv 1 src 0)
        newv))

    (define (vector-copy! dest dest-start src src-start)
      (letrec ((loop (lambda (dest-start src-start)
                      (if (not (or
                        (= dest-start (vector-length dest))
                          (= src-start (vector-length src))))
                        (begin (vector-set! dest dest-start (vector-ref src src-start))
                          (loop (+ 1 dest-start) (+ 1 src-start)))))))
              (loop dest-start src-start)))


(define (add-to-global-env binding evaled-body)
  (cases environment global-env
    [extended-env (syms vals env)
      (set! global-env (extended-env (cons binding syms)
        (vector-cons (box evaled-body) vals) env))]
    [else (eopl:error 'add-to-global-env "you done f**ked up")])
)

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))


(define (apply-env-with-global env sym)
   (unbox (apply-env-ref-with-global env sym)))

(define (apply-env-ref-with-global env sym)
 (apply-env-ref
    env
    sym
    (lambda (v) v); procedure to call if id is in env
      (lambda ()
         (if (c...r? (symbol->string sym))
             (box (prim-proc sym)) ;if it is a version of cadar then return that proc
             (apply-env-ref global-env
               sym
               (lambda (v) v)
               (lambda () (eopl:error 'apply-env "variable ~s is not bound" sym)))))))

(define (apply-env env depth position pass fail)
  (unbox (apply-env-ref env depth position pass fail)))

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are "callback procedures,
    (cases environment env       ;  succeed is appluied if sym is found, otherwise
      [empty-env ()       ;  fail is applied.
        (fail)]
      [extended-env (syms vals env)
    		(let ((pos (list-find-position sym syms)))
          	  (if (= 0 depth)
        				(vector-ref vals position)
        				(apply-env-ref env (- depth 1) position succeed fail)))])))
