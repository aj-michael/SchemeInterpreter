; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3


;(define empty-env
;  (lambda ()
;    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))
  ;  (cases environment env
 ;     [empty-env-record ()
 ;       (extended-env-record syms vals env)]
;      [extended-env-record (syms vals env2)
 ;       (extended-env-record syms vals env)]
  ;    [global-env-record (syms vals)
   ;     (extended-env-record syms vals env)])))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-ref
  (lambda (ls pos)
    (let loop ([ls ls][pos pos])
      (cond [(null? ls) (eopl:error 'list-ref "list index out of bounds")]
            [(zero? pos) (car ls)]
            [else (loop (cdr ls) (- pos 1))]))))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
             
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define set-list!
  (lambda (ls index val)
    (if (= index 0)
        (set-car! ls val)
        (let ([tail (list-tail ls (- index 1))])
          (set-cdr! tail (cons val (cddr tail)))))))

(define env-set!
  (lambda (env sym newval k)
    (cases environment env
;      [empty-env-record ()
;        (eopl:error 'set! "Cannot set a variable that is not defined")]
      [extended-env-record (syms vals env)
        (let ([pos (list-find-position sym syms)])
          (if (number? pos)
              (let ([val (list-ref vals pos)])
                (if (and (pair? val) (list? val) (eqv? (car val) 'ThisIsAReferenceTo))
                    (env-set! (caddr val) (cadr val) newval k)
                    (begin
                      (set-list! vals pos newval)
                      (apply-k k (void)))))
              (env-set! env sym newval k)))]
      ;        (apply-k k (void)))]
      [global-env-record (syms vals)
        (let ([pos (list-find-position sym syms)])
          (if (number? pos)
              (begin (set-list! vals pos newval)
                     (apply-k k (void)))
              (eopl:error 'set! "Cannot set a variable that is not defined")))])))

(define env-define!
  (lambda (env sym val k)
    (cases environment env
      [extended-env-record (syms vals env)
        (eopl:error 'env-define! "only global define allowed")]
      [global-env-record (syms vals)
          (if (list-find-position sym syms)
              (env-set! env sym val k)
              (begin
                (append! syms (list sym))
                (append! vals (list val))
                (apply-k k (void))))])))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
;      [empty-env-record ()
;        (fail)]
      [extended-env-record (syms vals env)
	    (let ((pos (list-find-position sym syms)))
          (if (and (number? pos) (not (null? vals)))
              (let ([val (list-ref vals pos)])
                (if (and (list? val) (pair? val) (eqv? (car val) 'ThisIsAReferenceTo))
                    (apply-env (caddr val) (cadr val) succeed fail)
                      (apply-k succeed (list-ref vals pos))))
              (cases environment env
                [extended-env-record (syms vals e2)
                  (apply-env env sym succeed fail)]
                [global-env-record (syms vals)
                  (apply-env env sym succeed fail)])))]
      [global-env-record (syms vals)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (apply-k succeed (list-ref vals pos))
              (apply-k (error-k "Variable undefined ~s" sym (rep)) '())))])))

