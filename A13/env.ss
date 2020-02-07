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
    [else (eopl:error 'add-to-global-env "you done f**ked up")]))

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

(define (apply-global-env sym pass fail)
   (unbox (apply-global-env-ref sym pass fail)))

(define (apply-global-env-ref sym pass fail)
   (cases environment global-env
     [empty-env () (fail)]
     [extended-env (syms vals env)
         (let ((pos (list-find-position sym syms)))
               (if (number? pos)
                 (pass (vector-ref vals pos))
                 (fail)))]))

(define (apply-env env depth position pass fail)
   (unbox (apply-env-ref env depth position pass fail)))

(define (apply-env-ref env depth position pass fail)
 (cases environment env
   [empty-env () (begin (display "empty env fail") (fail))]
   [extended-env (syms vals env)
       (if (= 0 depth)
         (vector-ref vals position)
         (apply-env-ref env (- depth 1) position pass fail))]))
