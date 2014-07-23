
;; this will replace symbols for idvars and varvars in lambda-exp
(define-datatype parameter parameter?
  (value-par
    (id symbol?))
  (reference-par
    (id symbol?)))

;; Parsed expression datatypes

(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
  (var-exp
    (id symbol?))
  (lit-exp
    (id (lambda (_var) (or (symbol? _var) (boolean? _var) (number? _var) (string? _var) (vector? _var) (list? _var) (eqv? _var #!eof)))))
  (lambda-exp
    (idvars (list-of symbol?))
    (varvars (list-of symbol?))
    (pass-ref (list-of boolean?))
    (bodies (list-of expression?)))
  (if-exp
    (condition expression?)
    (optionT expression?)
    (optionF expression?))
  (set!-exp
    (id symbol?)
    (body expression?))
  (let-exp
    (vars (list-of symbol?))
    (varbodies (list-of expression?))
    (bodies (list-of expression?)))
  (let*-exp
    (vars (list-of symbol?))
    (varbody (list-of expression?))
    (bodies (list-of expression?)))
  (letrec-exp
    (vars (list-of symbol?))
    (varbody (list-of expression?))
    (bodies (list-of expression?)))
  (begin-exp
    (bodies (list-of expression?)))
  (cond-exp
    (preds (list-of expression?))
    (bodies (list-of expression?)))
  (and-exp
    (preds (list-of expression?)))
  (or-exp
    (preds (list-of expression?)))
  (case-exp
    (condition expression?)
    (sets-of-vals (list-of expression?))
    (bodies (list-of expression?)))
  (app-exp
    (rator expression?)
    (rands (list-of expression?)))
  (while-exp
    (test expression?)
    (bodies (list-of expression?)))
  (define-exp
    (id symbol?)
    (body expression?))
  (define-datatype-exp
    (name symbol?)
    (pred symbol?)
    (records (list-of record?)))
  (cases-exp
    (datatype symbol?)
    (rec expression?)
    (rec-types (list-of symbol?))
    (args (list-of (list-of symbol?)))
    (exps (list-of expression?)))
  (record
    (name symbol?)
    (vals (list-of symbol?))
    (preds (list-of expression?))))

(define-datatype continuation continuation?
  [if-k ; recieves a boolean value 
    (then-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k continuation?)]
  [set!-k ; recieves a new value to set for id
    (id symbol?)
    (env environment?)
    (k continuation?)]
  [define-k ; recieves a value to be defined as id
    (id symbol?)
    (k continuation?)]
  [begin-k  ; ignores recieved values, for begin and multibodied expressions
    (bodies (list-of expression?))
    (env environment?)
    (k continuation?)]
  [proc-k  ; applies recieved value as a procedure to the list of args
    (rands (list-of expression?))
    (env environment?)
    (k continuation?)]
  [eval-app-k
    (proc proc-val?)
    (rands (list-of expression?))
    (args (list-of scheme-value?))
    (argnames (list-of symbol?))
    (env environment?)
    (k continuation?)]
  [map-k
    (proc proc-val?)
    (ls (list-of scheme-value?))
    (env environment?)
    (k continuation?)]
  [map-inner-k
    (k continuation?)
    (u scheme-value?)]

  
  [error-k  ; for displaying errors
    (message string?)
    (args (list-of scheme-value?))
    (k continuation?)]
  [return-k]
  [rep-k]  ; prints the recieved value for an rep cycle and starts the next one
  [exit-k])  ; for implementing exit


(define record?
  (lambda (exp)
    (cases expression exp
      (record (name vals preds) #t)
      (else #f))))
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (idvars (list-of symbol?))
    (varvars (list-of symbol?))
    (pass-ref (list-of boolean?))
    (bodies (list-of expression?))
    (env environment?)]
  [kont
    (k continuation?)])

;; environment type definitions
(define scheme-value?
  (lambda (x)
    #t))
    
(define-datatype environment environment?
  (global-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?)))
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)))
