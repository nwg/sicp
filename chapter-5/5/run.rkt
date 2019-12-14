#lang racket

(require "eceval.rkt")
(require "machine.rkt")
(require "compiler.rkt")
(require (only-in "interpreter.rkt" setup-environment))

(define the-global-environment
  (setup-environment))

;; (eceval 'trace-on)

(define (compile-and-go expression)
  (let-values ([(instructions labels) (assemble (statements (compile expression 'val 'return the-empty-compile-environment)) eceval)])
    ;; ((eceval 'trace-register) 'val)
    ;; ((eceval 'trace-register) 'arg2)
    (set! the-global-environment
          (setup-environment))

    (set-register-contents! 
     eceval 'val instructions)
    (set-register-contents! 
     eceval 'flag true)
    (start eceval)))

;;(extract-pkg-dependencies (get-info (list "run.rkt")))

(compile-and-go
 '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

;(compile
; '(define (factorial n)
;    (if (= n 1)
;        1
;        (* (factorial (- n 1)) n)))
; 'val 'return the-empty-compile-environment)
