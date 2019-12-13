#lang racket

(require "interpreter.rkt")
(require "machine.rkt")
(require "compiler.rkt")

(define the-global-environment
  (setup-environment))

;; (eceval 'trace-on)

(define (compile-and-go expression)
  (let-values ([(instructions labels) (assemble (statements (compile expression 'val 'return the-empty-compile-environment)) eceval)])
    (displayln (pair? instructions))
    ;; ((eceval 'trace-register) 'val)
    ;; ((eceval 'trace-register) 'arg2)
    (set! the-global-environment
          (setup-environment))

    (displayln "before set")
    (set-register-contents! 
     eceval 'val instructions)
    (displayln "after set")
    (set-register-contents! 
     eceval 'flag true)
    (start eceval)))

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

;(compile
; '(define (factorial n)
;    (if (= n 1)
;        1
;        (* (factorial (- n 1)) n)))
; 'val 'return the-empty-compile-environment)
