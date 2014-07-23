; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form k)
    ; later we may add things that are not expressions.
    (eval-exp form global-env k)))

; eval-exp is the main component of the interpreter

(define __isREP #f)

(print-graph #t)

(define print-proc
  (lambda (proc)
    (cases proc-val proc
      [prim-proc (name)
        (pretty-print proc)]
      [closure (idvars varvars pass-ref bodies env)
        (pretty-print
          (list 'closure
                (if (null? idvars)
                    (if (null? varvars)
                        '()
                        (car varvars))
                    (let get-args ([idvars idvars][pass-ref pass-ref])
                       (cond [(null? idvars) (if (null? varvars) '() (car varvars))]
                             [(car pass-ref)
                              (cons (list 'ref (car idvars))
                              (get-args (cdr idvars) (cdr pass-ref)))]
                             [else (cons (car idvars) (get-args (cdr idvars) (cdr pass-ref)))])))
                bodies
                (let get-envs ([env env])
                  (cases environment env
                    [extended-env-record (syms vals env)
                      (list syms vals get-envs env)]
                    [global-env-record (syms vals)
                      (list 'global-environment)]))))])))

(define interpreter-print
  (lambda (slist)
   (cond [(proc-val? slist)
          (print-proc slist)]
         [(eqv? slist (void))
          (void)]
         [else (pretty-print slist)])))

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id)
        (apply-env env id k
          (error-k "variable ~s is not bound" (list id) (rep-k)))]
      [app-exp (rator rands)
        (eval-exp rator env
          (proc-k rands env k))]
      [if-exp (condition optionT optionF)
        (eval-exp condition env
          (if-k optionT optionF env k))]
      [lambda-exp (idvars varvars pass-ref bodies)
        (apply-k k (closure idvars varvars pass-ref bodies env))]
      [set!-exp (id body)
        (eval-exp body env
          (set!-k id env k))]
      [define-exp (id body)
        (eval-exp body env
          (define-k id k))]
      [else (apply-k (error-k "Bad abstract syntax: ~a" (list exp) (rep-k)))])))

; evaluate the list of operands, putting results into a list
;(define eval-rands
;  (lambda (rands env)
;    (map (lambda(x) (eval-exp x env)) rands)))

(define name
  (lambda (exp)
    (cond [(expression? exp)
           (cases expression exp
             [var-exp (id) id]
             [else '_nameless])]
          [(proc-val? exp)
           (cases proc-val
             [prim-proc (id) id]
             [else '_nameless])]
          [else '_nameless])))

(define apply-k
  (lambda (k exp)
    (cases continuation k
      [if-k (then-exp else-exp env k)
        (if exp
          (eval-exp then-exp env k)
          (eval-exp else-exp env k))]
      [set!-k (id env k)
        (env-set! env id exp k)]
      [define-k (id k)
        (env-define! global-env id exp k)]
      [begin-k (bodies env k)
        (if (null? (cdr bodies))
          (eval-exp (car bodies) env k)
          (eval-exp (car bodies) env (begin-k (cdr bodies) env k)))]
      [proc-k (rands env k)
        (if (null? rands)
            (apply-proc exp '() '() env k)
            (eval-exp (car rands) env
              (eval-app-k exp (cdr rands) '() (list (name (car rands))) env k)))]
      [eval-app-k (proc rands erands randnames env k)
        (if (null? rands)
            (apply-proc proc (reverse (cons exp erands)) (reverse randnames) env k)
            (eval-exp (car rands) env
              (eval-app-k proc (cdr rands) (cons exp erands) (cons (name (car rands)) randnames) env k)))]
      [error-k (message args k)
        (begin
          (apply printf message args)
          (apply-k k (void)))]
      [rep-k () 
        (begin
          (interpreter-print exp)
          (rep))]
      [return-k ()
        exp]
      [map-k (proc ls env k)
        (prim-proc-map proc (cdr ls) env (map-inner-k k exp))]
      [map-inner-k (k u)
        (apply-k k (cons u exp))]
      [exit-k () (void)])))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define apply-proc
  (lambda (proc-value args argnames calling-env k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args calling-env k)]
      [closure (idvars varvars pass-ref bodies env)
        (let ([extended-env
                (extend-env
                  (append idvars varvars)
                  (let format-vars ([ls args]
                                    [n (length idvars)]
                                    [refs pass-ref]
                                    [names argnames])
                    (cond
                      ((null? ls) ls)
                      ((= n 0) (list ls))
                      (else
                        (cons
                          (if (and (list? refs) (pair? refs) (car refs))
                              (list 'ThisIsAReferenceTo (cadar names) calling-env)
                              (car ls))
                          (format-vars (cdr ls) (- n 1) (cdr refs) (cdr names))))))
                  env)])
          (eval-exp (car bodies) extended-env
            (if (null? (cdr bodies))
                k
                (begin-k (cdr bodies) extended-env k))))]
      [kont (k)
        (apply-k k (car args))]
      [else (apply-k (error-k "attempt to apply bad procedure: ~s" (list proc-value) (rep-k)))])))

(define *prim-proc-names* '(void not + - * / < > = <= >= add1 sub1 list null? procedure? assq eq? equal? atom? length list->vector list? pair? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr zero? apply map member quotient list-tail append eqv? load call-with-input-file list-copy read print-graph pretty-print boolean? string? andmap display printf call/cc call-with-current-continuation exit));

(define global-env
  (let ([ppns (list-copy *prim-proc-names*)])
       (global-env-record
         ppns
         (map prim-proc
           ppns))))

; I have to use list-copy, the other ways didn't work
(define reset-global-env
  (lambda ()
    (let*([ppns (list-copy *prim-proc-names*)]
          [pps (map prim-proc ppns)])
      (cases environment global-env
        (global-env-record (syms vals)
          (set-car! syms (car ppns))
          (set-cdr! syms (cdr ppns))
          (set-car! vals (car pps))
          (set-cdr! vals (cdr pps)))
        (else (eopl:error 'reset-global-env "global-env is wrong type"))))))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args env k)
    (case prim-proc
		[(void) (apply-k k (void))]
        [(not) (apply-k k (not (1st args)))]
		[(+) (apply-k k (apply + args))]
		[(-) (apply-k k (apply - args))]
		[(*) (apply-k k (apply * args))]
		[(/) (apply-k k (/ (1st args) (2nd args)))]
		[(<) (apply-k k (< (1st args) (2nd args)))]
		[(>) (apply-k k (> (1st args) (2nd args)))]
		[(=) (apply-k k (= (1st args) (2nd args)))]
		[(<=)(apply-k k (<= (1st args) (2nd args)))]
		[(>=)(apply-k k (>= (1st args) (2nd args)))]
        [(zero?)(apply-k k  (zero? (1st args)))]
		[(add1) (apply-k k (+ (1st args) 1))]
		[(sub1) (apply-k k (- (1st args) 1))]
		[(list) (apply-k k args)]
		[(null?) (apply-k k (null? (1st args)))]
        [(procedure?) (apply-k k (proc-val? (1st args)))]
		[(assq) (apply-k k (assq (1st args) (2nd args)))]
		[(eq?) (apply-k k (eq? (1st args) (2nd args)))]
		[(equal?) (apply-k k (equal? (1st args) (2nd args)))]
		[(atom?) (apply-k k (atom? (1st args)))]
        [(string?) (apply-k k (string? (1st args)))]
        [(display) (apply-k k (display (1st args)))]
		[(length) (apply-k k (length (1st args)))]
		[(list->vector) (apply-k k (list->vector (1st args)))]
		[(list?) (apply-k k (list? (1st args)))]
		[(pair?) (apply-k k (pair? (1st args)))]
		[(vector->list) (apply-k k (vector->list (1st args)))]
		[(vector) (apply-k k (apply vector args))]
		[(make-vector) (apply-k k (make-vector (1st args) (2nd args)))]
		[(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
		[(vector?) (apply-k k (vector? (1st args)))]
		[(number?) (apply-k k (number? (1st args)))]
		[(symbol?) (apply-k k (symbol? (1st args)))]
		[(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
		[(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
		[(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
		[(display) (apply-k k (display (1st args)))]
		[(newline) (apply-k k (newline))]
		[(cons) (apply-k k (cons (1st args) (2nd args)))]
		[(car) (apply-k k (car (1st args)))]
		[(cdr) (apply-k k (cdr (1st args)))]
		[(caar) (apply-k k (caar (1st args)))]
		[(cadr) (apply-k k (cadr (1st args)))]
		[(cdar) (apply-k k (cdar (1st args)))]
		[(cddr) (apply-k k (cddr (1st args)))]
		[(caaar) (apply-k k (caaar (1st args)))]
		[(caadr) (apply-k k (caadr (1st args)))]
		[(cadar) (apply-k k (cadar (1st args)))]
		[(caddr) (apply-k k (caddr (1st args)))]
		[(cdaar) (apply-k k (cdaar (1st args)))]
		[(cdadr) (apply-k k (cdadr (1st args)))]
		[(cddar) (apply-k k (cddar (1st args)))]
		[(cdddr) (apply-k k (cdddr (1st args)))]
		[(caaaar) (apply-k k (caaaar (1st args)))]
		[(caaadr) (apply-k k (caaadr (1st args)))]
		[(caadar) (apply-k k (caadar (1st args)))]
		[(caaddr) (apply-k k (caaddr (1st args)))]
		[(cadaar) (apply-k k (cadaar (1st args)))]
		[(cadadr) (apply-k k (cadadr (1st args)))]
		[(caddar) (apply-k k (caddar (1st args)))]
		[(cadddr) (apply-k k (cadddr (1st args)))]
		[(cdaaar) (apply-k k (cdaaar (1st args)))]
		[(cdaadr) (apply-k k (cdaadr (1st args)))]
		[(cdadar) (apply-k k (cdadar (1st args)))]
		[(cdaddr) (apply-k k (cdaddr (1st args)))]
    		[(cddaar) (apply-k k (cddaar (1st args)))]
    		[(cddadr) (apply-k k (cddadr (1st args)))]
    		[(cdddar) (apply-k k (cdddar (1st args)))]
    		[(cddddr) (apply-k k (cddddr (1st args)))]
      [(boolean?) (apply-k k (boolean? (1st args)))]
      [(member) (apply-k k (apply prim-proc-member args))]
      [(apply) (apply-proc (1st args) (2nd args) (map (lambda (x) #f) (2nd args)) env k)]
      [(map) (prim-proc-map (1st args) (2nd args) env k)]
      [(quotient) (apply-k k (quotient (1st args) (2nd args)))]
      [(append) (apply-k k (append (1st args) (2nd args)))]
      [(list-tail) (apply-k k (list-tail (1st args) (2nd args)))]
      [(eqv?) (apply-k k (eqv? (1st args) (2nd args)))]
      [(load) (apply-k k (read-file (1st args)))]
      [(call-with-input-file) (apply-k k (apply call-with-input-file args))]
      [(read) (apply-k k (apply read args))]
      [(list-copy) (apply-k k (list-copy (1st args)))]
      [(print-graph) (apply-k k (print-graph (1st args)))]
      [(pretty-print) (apply-k k (pretty-print (1st args)))]
      [(printf) (apply-k k (apply printf args))]
      [(andmap) (apply-k k (prim-proc-andmap (1st args) (cdr args)))]
      [(call/cc) (apply-proc (1st args) (list (kont k)) (list #f) env k)]
      [(call-with-current-continuation) ((1st args) k)]
      [(exit) (apply-k (if __isREP (rep-k) (return-k)) args)] 
      [else (error 'apply-prim-proc 
				"Bad primitive procedure name: ~s" 
				prim-op)])))

(define prim-proc-andmap
  (lambda (proc args)
    (if (null? (car args))
        #t
        (let ([proc-args (map car args)])
             (if (apply-proc proc proc-args (map (lambda (x) #f) proc-args) env)
                 (prim-proc-andmap (map cdr args))
                 #f)))))

(define prim-proc-member
  (lambda (item ls)
    (cond
      [(null? ls) #f]
      [(eqv? (car ls) item) ls]
      [else (prim-proc-member item (cdr ls))])))

(define prim-proc-map
  (lambda (proc ls env k)
    (if (null? ls)
        (apply-k k '())
        (apply-proc proc ls (map (lambda (x) 'NotALambdaSoWeDontNeedNames) ls) env
          (map-k proc ls env k)))))
        
(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda(input)
        (let loop ([slist (read input)])
          (if (eqv? slist '#!eof)
              (void)
              (begin
                (eval-exp (syntax-expand (parse-exp slist)) global-env)
                (loop (read input)))))))))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (set! __isREP #t)
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (top-level-eval (syntax-expand (parse-exp (read))) (rep-k))))

(define eval-one-exp
  (lambda (x) (set! __isREP #f) (top-level-eval (syntax-expand (parse-exp x)) (return-k))))

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      (lambda-exp (idvars varvars pass-ref bodies)
        (lambda-exp idvars varvars pass-ref (map syntax-expand bodies)))
      (app-exp (rator rands)
        (app-exp (syntax-expand rator) (map syntax-expand rands)))
      (let-exp (vars varbody bodies)
        (app-exp
          (lambda-exp vars '() (map (lambda (x) #f) vars) (map syntax-expand bodies))
          (map syntax-expand varbody)))
      (begin-exp (bodies)
        (app-exp (lambda-exp '() '() '() (map syntax-expand bodies)) '()))
      (cond-exp (preds bodies)
        (if-exp (syntax-expand (car preds))
          (syntax-expand (car bodies))
          (if (not (null? (cdr preds)))
              (syntax-expand (cond-exp (cdr preds) (cdr bodies)))
              (app-exp (var-exp 'void) '()))))
      (if-exp (condition optionT optionF)
        (if-exp (syntax-expand condition) (syntax-expand optionT) (syntax-expand optionF)))
      (and-exp (preds)
        (if (null? preds)
            (lit-exp #t)
            (syntax-expand
              (let-exp
                (list '_temp)
                (list (syntax-expand (car preds)))
                (list
                  (if-exp (parse-exp '(not '_temp))
                    (lit-exp #f)
                    (if (null? (cdr preds))
                        (var-exp '_temp)
                        (syntax-expand (and-exp (cdr preds))))))))))
      (or-exp (preds)
        (if (null? preds)
            (lit-exp #f)
            (syntax-expand
              (let-exp
                (list '_temp)
                (list (syntax-expand (car preds)))
                (list
                  (if-exp (var-exp '_temp)
                    (var-exp '_temp)
                    (if (null? (cdr preds))
                        (lit-exp #f)
                        (syntax-expand (or-exp (cdr preds))))))))))
      (case-exp (condition sets-of-vals bodies)
        (syntax-expand
          (let-exp
            (list '_temp)
            (list (syntax-expand condition))
            (list
              (let loop ([vals sets-of-vals]
                         [bodies bodies])
                (if (eqv? 'else (cases expression (car vals) (lit-exp (id) id) (else (eopl:error 'syntax-expand "case values must be literals"))))
                    (car bodies)
                    (if-exp (app-exp (var-exp 'member) (list (var-exp '_temp) (car vals)))
                      (car bodies)
                      (if (null? (cdr bodies))
                          (app-exp (var-exp 'void) '())
                          (loop (cdr vals) (cdr bodies))))))))))
      (while-exp (test-exp bodies)
        (app-exp
          (app-exp
            (lambda-exp
              '(proc)
              '()
              '(#f)
              (list
                (lambda-exp
                  '(test-proc)
                  '()
                  '(#f)
                  (list
                    (if-exp
                      (app-exp (var-exp 'test-proc) '())
                      (syntax-expand
                        (begin-exp
                          (append
                            bodies
                            (list
                              (app-exp
                                (app-exp (var-exp 'proc) (list (var-exp 'proc)))
                                (list (var-exp 'test-proc)))))))
                      (app-exp (var-exp 'void) '()))))))
            (list
              (lambda-exp
                '(proc)
                '()
                '(#f)
                (list
                  (lambda-exp
                    '(test-proc)
                    '()
                    '(#f)
                    (list
                      (if-exp
                        (app-exp (var-exp 'test-proc) '())
                        (syntax-expand
                          (begin-exp
                            (append
                              bodies
                              (list
                                (app-exp
                                  (app-exp (var-exp 'proc) (list (var-exp 'proc)))
                                  (list (var-exp 'test-proc)))))))
                        (app-exp (var-exp 'void) '()))))))))
          (list (lambda-exp '() '() '() (list test-exp)))))
      (letrec-exp (vars varbodies bodies)
        (syntax-expand
          (app-exp
            (var-exp 'apply)
            (list
              (lambda-exp vars '() (map (lambda (x) #f) vars) bodies)
              (app-exp
                (parse-exp
                  '(lambda L
                     ((lambda (f) (f f))
                      (lambda (g)
                        (map
                          (lambda (f)
                            (lambda L
                              (apply
                                (apply f (g g)) L))) L)))))
                (map (lambda (body) (lambda-exp vars '() (map (lambda (x) #f) vars) (list body))) varbodies))
              ))))
      (let*-exp (vars varbodies bodies)
        (if (null? vars)
            (app-exp
              (lambda-exp '() '() '() (map syntax-expand bodies))
              '())
            (syntax-expand
              (let-exp
                (list (car vars))
                (list (car varbodies))
                (list
                  (let*-exp
                    (cdr vars)
                    (cdr varbodies)
                    bodies))))))
      (define-exp (id body)
        (define-exp id
          (syntax-expand body)))
      [cases-exp (datatype rec rec-types argss exps)
        (syntax-expand
          (case-exp (app-exp (var-exp 'car) (list rec))
                    (map (lambda(type)(if (eqv? type 'else) (lit-exp 'else) (lit-exp (list type)))) rec-types)
                    (map (lambda(exp args)
                         (app-exp
                            (var-exp 'apply)
                            (list (lambda-exp args '() (map (lambda(x)#f) args)
                                    (list exp))
                                  (app-exp (var-exp 'cdr) (list rec)))))
                           exps
                           argss)))]
      (define-datatype-exp (id pred records)
        (let ([record-names (map (lambda(x)(cases expression x (record (name vals preds) name)  (else eopl:error))) records)]
              [record-vals  (map (lambda(x)(cases expression x (record (name vals preds) vals)  (else eopl:error))) records)]
              [record-preds (map (lambda(x)(cases expression x (record (name vals preds) preds) (else eopl:error))) records)])
          (syntax-expand
            (begin-exp
              (append
                (list (define-exp id (lit-exp record-names))
                      (define-exp pred
                        (lambda-exp '(x) '() '(#f)
                          (list
                            (cases-exp id (var-exp 'x) record-names record-vals
                              (map (lambda(args preds)(and-exp (map app-exp preds (map list (map var-exp args)))))
                                   record-vals
                                   record-preds))))))
                (map define-exp
                     record-names
                     (map lambda-exp
                          record-vals
                          (map (lambda(x)'()) record-vals)
                          (map (lambda(x)(map (lambda(x)#f) x)) record-vals)
                          (map (lambda(name args)
                                      (list (app-exp (var-exp 'list)
                                                     (cons (lit-exp name)
                                                           (map var-exp args)))))
                               record-names
                               record-vals))))))))
      (else
        exp))))

