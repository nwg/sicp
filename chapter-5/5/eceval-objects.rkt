#lang racket

(require "../../utility.rkt")
(require compatibility/mlist)

(provide (all-defined-out))

(define environment-max-vars 5)
(define (display-string-for-environment env)
  (cond [(empty-environment? env)
         "<empty>"]
        [(= (length env) 1) "<global>"]
        [else
         (string-append
          "<"
          (truncate-for-display (frame-variables (first-frame env)) "/" environment-max-vars)
          "> + "
          (~s (- (length env) 1)))]))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (environment? env)
  (or
   (empty-environment? env)
   (frame? (first-frame env))))
(define (empty-environment? env) (eq? env the-empty-environment))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons 'frame (mcons variables (list->mlist values))))
(define (frame? val) (tagged-list? val 'frame))
(define (frame-pairs frame) (cdr frame))
(define (frame-variables frame) (mcar (frame-pairs frame)))
(define (frame-values frame) (mcdr (frame-pairs frame)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) 
  (cadr c-proc))
(define (compiled-procedure-env c-proc)
  (caddr c-proc))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))

