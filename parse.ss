; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      (((lambda (_var) (or (boolean? _var) (number? _var) (string? _var) (vector? _var) (eqv? _var #!eof))) datum) (lit-exp datum))
      ((list? datum)
       (cond
         ((eqv? (car datum) 'lambda)
          (cond
            ((< (length datum) 3)
             (eopl:error 'parse-exp "Incomplete lambda ~s" datum))
            ((list? (cadr datum))
             (if (andmap (lambda (val) (or (symbol? val) (list? val))) (cadr datum))
                 (lambda-exp
                   (map (lambda (var-clause) (if (list? var-clause) (cadr var-clause) var-clause)) (cadr datum))
                   '() 
                   (map list? (cadr datum))
                   (map parse-exp (cddr datum)))
                 (eopl:error 'parse-exp "Formal arguments must be symbols ~s" datum)))
            ((pair? (cadr datum))
             (let ([formals (let improper-helper ([lst (cadr datum)])
                              (if (pair? lst) (cons (car lst) (improper-helper (cdr lst))) '()))]
                   [improper (let improper-helper2 ([lst (cadr datum)]) (if (pair? lst) (improper-helper2 (cdr lst)) lst))])
               (if (and (andmap symbol? formals) (symbol? improper))
                   (lambda-exp
                     formals
                     (list improper)
                     (cons #f (map (lambda (x) #f) formals))
                     (map parse-exp (cddr datum)))
                   (eopl:error 'parse-exp "Formal arguments must be symbols ~s" datum))))
            ((symbol? (cadr datum))
             (lambda-exp '() (list (cadr datum)) (list #f) (map parse-exp (cddr datum))))
            (else
              (lambda-exp 'normal (cadr datum)
                (map parse-exp (cddr datum))))))
         ((eqv? (car datum) 'if)
          (cond
            ((< (length datum) 3)
             (eopl:error 'parse-exp "if expression: missing body ~s" datum))
            ((= 3 (length datum))
			 (if-exp (parse-exp (cadr datum))
			    (parse-exp (caddr datum))
				(parse-exp '(void))))
			(else 
              (if-exp (parse-exp (cadr datum))
                (parse-exp (caddr datum))
                (parse-exp (cadddr datum))))))
         ((eqv? (car datum) 'set!)
          (cond
            ((and (symbol? (cadr datum)) (= 3 (length datum)))
             (set!-exp (cadr datum) (parse-exp (caddr datum))))
            (else
              (eopl:error 'parse-exp "illegal set! expression ~s" datum))))
         ((eqv? (car datum) 'let)
          (cond
            ((not (list? (cadr datum)))
             (if (list? (caddr datum))
                 (letrec-exp
                   (list (cadr datum))
                   (list (lambda-exp (map car (caddr datum)) '() (map (lambda (x) #f) (caddr datum)) (map parse-exp (cdddr datum))))
                   (list (app-exp (var-exp (cadr datum)) (map (lambda (x) (parse-exp (cadr x))) (caddr datum)))))
                 (eopl:error 'parse-exp "let declarations not a list ~s" datum)))
            ((< (length datum) 3)
             (eopl:error 'parse-exp "let expression is missing body ~s" datum))
            ((not (andmap list? (cadr datum)))
             (eopl:error 'parse-exp "let declarations not all lists ~s" datum))
            ((not (andmap (lambda (ls) (= 2 (length ls))) (cadr datum)))
             (eopl:error 'parse-exp "let declarations not all length 2 ~s" datum))
            ((not (andmap symbol? (map car (cadr datum))))
             (eopl:error 'parse-exp "first elements must be symbols ~s" datum))
            (else
             (let-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum))))))
         ((eqv? (car datum) 'let*)
          (cond
            ((not (list? (cadr datum)))
             (eopl:error 'parse-exp "let* declarations not a list ~s" datum))
            ((< (length datum) 3)
             (eopl:error 'parse-exp "let* expression is missing body ~s" datum))
            ((not (andmap list? (cadr datum)))
             (eopl:error 'parse-exp "let* declarations not all lists ~s" datum))
            ((not (andmap (lambda (ls) (= 2 (length ls))) (cadr datum)))
             (eopl:error 'parse-exp "let* declarations not all length 2 ~s" datum))
            ((not (andmap symbol? (map car (cadr datum))))
             (eopl:error 'parse-exp "first elements must be symbols ~s" datum))
            (else
             (let*-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum))))))
         ((eqv? (car datum) 'letrec)
          (cond
            ((not (list? (cadr datum)))
             (eopl:error 'parse-exp "letrec declarations not a list ~s" datum))
            ((< (length datum) 3)
             (eopl:error 'parse-exp "letrec expression is missing body ~s" datum))
            ((not (andmap list? (cadr datum)))
             (eopl:error 'parse-exp "letrec declarations not all lists ~s" datum))
            ((not (andmap (lambda (ls) (= 2 (length ls))) (cadr datum)))
             (eopl:error 'parse-exp "letrec declarations not all length 2 ~s" datum))
            ((not (andmap symbol? (map car (cadr datum))))
             (eopl:error 'parse-exp "first elements must be symbols ~s" datum))
            (else
             (letrec-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum))))))         
         ((eqv? (car datum) 'quote)
          (cond
            ((not (= 2 (length datum)))
             (eopl:error 'parse-exp "quote must have one argument ~s" datum))
            (else
              (lit-exp (cadr datum)))))
         ((eqv? (car datum) 'begin)
          (begin-exp (map parse-exp (cdr datum))))
         ((eqv? (car datum) 'cond)
          (cond-exp
            (map (lambda (x) (if (eqv? (car x) 'else)
                                 (parse-exp #t)
                                 (parse-exp (car x))))
              (cdr datum))
            (map (lambda (x) (parse-exp (cadr x))) (cdr datum))))
         ((eqv? (car datum) 'and)
          (and-exp (map parse-exp (cdr datum))))
         ((eqv? (car datum) 'or)
          (or-exp (map parse-exp (cdr datum))))
         ((eqv? (car datum) 'case)
          (case-exp
            (parse-exp (cadr datum))
            (map (lambda (x) (lit-exp (car x))) (cddr datum))
            (map (lambda (x) (parse-exp (cadr x))) (cddr datum))))
         ((eqv? (car datum) 'while)
          (while-exp
            (parse-exp (cadr datum))
            (map parse-exp (cddr datum))))
         ((eqv? (car datum) 'define)
          (define-exp (cadr datum) (parse-exp (caddr datum))))
         ((eqv? (car datum) 'define-datatype)
          (let ([name (cadr datum)]
                [pred (caddr datum)]
                [records (cdddr datum)])
            (define-datatype-exp
              name
              pred
              (map record (map car records)
                          (map (lambda(x)(map car x)) (map cdr records))
                          (map (lambda(x)(map parse-exp (map cadr x))) (map cdr records))))))
         ((eqv? (car datum) 'cases)
          (let ([datatype (cadr datum)]
                [rec (caddr datum)]
                [types (cdddr datum)])
            (cases-exp
              datatype
              (parse-exp rec)
              (map car types)
              (map (lambda(x)(if (null? (cddr x)) '() (cadr x)))types)
              (map (lambda(x)(if (null? (cddr x)) (parse-exp (cadr x))(parse-exp (caddr x))))types))))
         (else
           (if (list? datum)
               (app-exp
                 (parse-exp (car datum))
                 (map parse-exp (cdr datum)))
               (eopl:error 'parse-exp "Application ~s is not a proper list" datum)))))
      (else (eopl:error 'parse-exp
              "Invalid concrete syntax ~s" datum)))))
  
(define unparse-exp ; an inverse for parse-exp
  (lambda (exp)
    (cases expression exp
      (var-exp (id) id)
      (lit-exp (id) id)
      (lambda-exp (idvars varvar pass-ref body)
        (cond
          ((null? varvar)
           (cons 'lambda (cons idvars (map unparse-exp body))))
          ((null? idvars)
           (cons 'lambda (cons (car varvar) (map unparse-exp body))))
          (else
            (cons 'lambda (cons (fold-right cons (car varvar) idvars) (map unparse-exp body))))))
      (if-exp (condition optionT optionF)
        (list 'if
          (unparse-exp condition)
          (unparse-exp optionT) 
          (unparse-exp optionF)))
      (set!-exp (id body)
        (list 'set! id
          (unparse-exp body)))
      (let-exp (vars varbody body)
        (cons 'let (cons 
                     (map list vars
                       (map unparse-exp varbody))
                     (map unparse-exp body))))
      (let*-exp (vars varbody body)
        (cons 'let* (cons 
                      (map list vars
                        (map unparse-exp varbody))
                      (map unparse-exp body))))
      (letrec-exp (vars varbody body)
        (cons 'letrec (cons 
                        (map list vars
                          (map unparse-exp varbody))
                        (map unparse-exp body))))
      (begin-exp (bodies)
        (cons 'begin (map unparse-exp bodies)))
;      (cond-exp (preds bodies)
;        (list 'IDidn'tImplementThis))
;      (and-exp (preds)
;        (list 'IDidn'tImplementThis))
;      (or-exp (preds)
;        (list 'IDidn'tImplementThis))
;      (case-exp (condition sets-of-vals bodies)
;        (list 'IDidn'tImplementThis))
;      (while-exp (test bodies)
;        (list 'IDidn'tImplementThis))
 ;     (define-exp (a b)
 ;       (list 'IDidn'tImplementThis))
      (app-exp (rator rands)
        (cons (unparse-exp rator)
          (map unparse-exp rands)))
      (else '(IDidn'tImplementThis)))))





