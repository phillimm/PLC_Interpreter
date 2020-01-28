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
  (apply-env
          env
          sym
          (lambda (v) v); procedure to call if id is in env
          (lambda ()
             (if (c...r? (symbol->string sym))
               (box (prim-proc sym)) ;if it is a version of cadar then return that proc
             (apply-env global-env
               sym
               (lambda (v) v)
               (lambda () (eopl:error 'apply-env "variable ~s is not bound" id)))))))

(define (apply-env env sym pass fail)
  (unbox (apply-env-ref env sym pass fail)))

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are "callback procedures,
    (cases environment env       ;  succeed is appluied if sym is found, otherwise
      [empty-env ()       ;  fail is applied.
        (fail)]
      [extended-env (syms vals env)
    		(let ((pos (list-find-position sym syms)))
          	  (if (number? pos)
        				(succeed (vector-ref vals pos))
        				(apply-env env sym succeed fail)))])))
