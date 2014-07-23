; Dan Hanson and Adam Michael

(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "interpreter.ss")
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "interpreter.ss")))

(load-all)

(define l load-all) ; even easier!
