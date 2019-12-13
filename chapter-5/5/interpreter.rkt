#lang racket

(require "../../utility.rkt")
(require compatibility/mlist)
(require "syntax.rkt")
(provide (all-defined-out))

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

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables (list->mlist values)))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define (lexical-address-lookup address env)
  (let* ([env-pos (car address)]
         [var-pos (cadr address)]
         [frame (list-ref env env-pos)]
         [values (frame-values frame)]
         [value (mlist-ref values var-pos)])
    (if (eq? value '*unassigned*)
        (error "lexical-address-lookup:: Unassigned value at address" address)
        value)))

(define (lexical-address-set! address val env)
  (let* ([env-pos (car address)]
         [var-pos (cadr address)]
         [frame (list-ref env env-pos)]
         [values (frame-values frame)]
         [back (mlist-tail values var-pos)])
    (set-mcar! back val)))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '* *)
        (list '- -)
        (list '> >)
        (list '< <)
        (list '+ +)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) 
                        (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display 
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) 
                        (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define (true? x)
  (not (eq? x false)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define the-global-environment
  (setup-environment))

(define (get-global-environment)
  the-global-environment)


;(start eceval)


;; (let* ([frame1 (extend-environment '(x y) '(2 3) the-empty-environment)]
;;        [frame2 (extend-environment '(z w) '(4 5) frame1)])
;;   (displayln (lexical-address-lookup '(1 1) frame2))
;;   (displayln (lexical-address-lookup '(0 0) frame2)))

;; (let* ([env1 (extend-environment
;;               '(x y) '(1 2) the-empty-environment)]
;;        [env2 (extend-environment
;;               '(z w) '(3 4) env1)])
;;   (lexical-address-set! '(1 1) 9 env2)
;;   (lexical-address-set! '(1 0) '*unassigned* env2)
;;   (displayln (lexical-address-lookup '(1 1) env2))
;;   (displayln (lexical-address-lookup '(1 0) env2))
;;   )
