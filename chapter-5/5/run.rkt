#lang racket

(require "eceval.rkt")
(require "machine.rkt")
(require "compiler.rkt")
(require "metacircular.rkt")
(require (only-in "eceval.rkt" setup-environment))
(require "metacircular.rkt")

(define the-global-environment
  (setup-environment))

(define eceval (make-eceval))
;(eceval 'trace-on)
;((eceval 'trace-register) 'env)
;((eceval 'trace-register) 'proc)

(define (compile-and-go expression)
  (let ([text (statements (compile expression 'val 'return the-empty-compile-environment))])
    (define out (open-output-file "compiler.out" #:mode 'text #:exists 'truncate))
    (for ([line text])
      (writeln line out))
    (close-output-port out)
    (let-values ([(instructions labels) (assemble text eceval)])
      (set! the-global-environment
            (setup-environment))

      (set-register-contents! 
       eceval 'val instructions)
      (set-register-contents! 
       eceval 'flag true)
      (start eceval))))

(compile-and-go
 metacircular)

;; (compile-and-go
;;  '(define (fib n)
;;     (define (f m k0 k1)
;;       (if (= m n)
;;           k0
;;           (f (+ m 1) k1 (+ k0 k1))))
;;     (f 0 0 1)))
