#lang racket

(provide metacircular)

(define metacircular 
  '(begin

     (define *UNASSIGNED-VALUE* '*unassigned-metacircular*)
     
     (define (map f seq)
       (if (null? seq) '()
           (cons (f (car seq)) (map f (cdr seq)))))

     (define (map2 f seq1 seq2)
       (if (null? seq1)
           '()
           (cons (f (car seq1) (car seq2)) (map2 f (cdr seq1) (cdr seq2)))))

     (define (findf f seq)
       (if (null? seq)
           false
           (if (f (car seq))
               (car seq)
               (findf f (cdr seq)))))

     
     (define (takef seq f)
       (define (accum items seq)
         (if (null? seq)
             items
             (if (f (car seq))
                 (accum (cons (car seq) items) (cdr seq))
                 items)))
       (reverse (accum '() seq)))

     (define (dropf seq f)
       (if (null? seq)
           '()
           (if (f (car seq))
               (dropf (cdr seq) f)
               seq)))

     (define (list-of-values-forward exps env)
       (if (no-operands? exps)
           '()
           (let ((first (eval (first-operand exps) env)))        
             (cons first
                   (list-of-values-forward 
                    (rest-operands exps) 
                    env)))))

     (define (list-of-values-reverse exps env)
       (if (no-operands? exps)
           '()
           (let ((second (list-of-values-reverse (rest-operands exps) env)))
             (cons (eval (first-operand exps) env)
                   second))))

     (define (self-evaluating? exp)
       (cond ((number? exp) true)
             ((string? exp) true)
             ((eq? exp '*UNASSIGNED-VALUE*) true)
             (else false)))

     (define (variable? exp) (symbol? exp))

     (define (quoted? exp)
       (tagged-list? exp 'quote))

     (define (text-of-quotation exp)
       (cadr exp))

     (define (tagged-list? exp tag)
       (if (pair? exp)
           (eq? (car exp) tag)
           false))

     (define (assignment? exp)
       (tagged-list? exp 'set!))

     (define (make-assignment var value)
       (list 'set! var value))

     (define (assignment-variable exp) 
       (cadr exp))
     
     (define (assignment-value exp) (caddr exp))

     (define (definition? exp)
       (tagged-list? exp 'define))

     (define (make-define name value)
       (list 'define name value))

     (define (definition-variable exp)
       (if (symbol? (cadr exp))
           (cadr exp)
           (caadr exp)))

     (define (definition-value exp)
       (if (symbol? (cadr exp))
           (caddr exp)
           (make-lambda 
            (cdadr exp)   ; formal parameters
            (cddr exp)))) ; body

     (define (scan-out-defines body)
       (let ([defines (takef body definition?)]
             [rest (dropf body definition?)])
         (if (null? defines)
             rest
             (if (findf definition? rest)
                 (error "Definitions must come first in body")
                 (let* ([vars (map definition-variable defines)]
                        [values (map definition-value defines)]
                        [let-definitions (map (lambda (var) (list var '*UNASSIGNED-VALUE*)) vars)]
                        [sets (map2 (lambda (var value) (make-assignment var value)) vars values)])
                   (list
                    (make-let
                     let-definitions
                     (append
                      sets
                      rest))))))))

     (define (lambda? exp) 
       (tagged-list? exp 'lambda))
     (define (lambda-parameters exp) (cadr exp))
     (define (lambda-body exp) (cddr exp))

     (define (make-lambda parameters body)
       (cons 'lambda (cons parameters body)))

     (define (if? exp) (tagged-list? exp 'if))
     (define (if-predicate exp) (cadr exp))
     (define (if-consequent exp) (caddr exp))
     (define (if-alternative exp)
       (if (not (null? (cdddr exp)))
           (cadddr exp)
           'false))

     (define (make-if predicate 
                      consequent 
                      alternative)
       (list 'if 
             predicate 
             consequent 
             alternative))


     (define (begin? exp) 
       (tagged-list? exp 'begin))
     (define (begin-actions exp) (cdr exp))
     (define (last-exp? seq) (null? (cdr seq)))
     (define (first-exp seq) (car seq))
     (define (rest-exps seq) (cdr seq))

     (define (sequence->exp seq)
       (cond ((null? seq) seq)
             ((last-exp? seq) (first-exp seq))
             (else (make-begin seq))))

     (define (make-begin seq) (cons 'begin seq))

     (define (application? exp) (pair? exp))
     (define (operator exp) (car exp))
     (define (operands exp) (cdr exp))
     (define (no-operands? ops) (null? ops))
     (define (first-operand ops) (car ops))
     (define (rest-operands ops) (cdr ops))

     (define (let? exp)
       (tagged-list? exp 'let))


     (define (named-let? exp)
       (and
        (tagged-list? exp 'let)
        (symbol? (cadr exp))))

     (define (named-let-name exp)
       (if (not (named-let? exp))
           (error "not a named let")
           (cadr exp)))

     (define (let-definitions exp)
       (if (named-let? exp)
           (caddr exp)
           (cadr exp)))

     (define (let-actions exp)
       (if (named-let? exp)
           (cdddr exp)
           (cddr exp)))

     (define (make-let definitions actions)
       (cons 'let (cons definitions actions)))

     (define (let->combination exp)
       (let* ([definitions (let-definitions exp)]
              [vars (map car definitions)]
              [initials (map cadr definitions)]
              [body (let-actions exp)])
         (if (named-let? exp)
             (begin
               (make-let
                '()
                (list
                 (make-begin
                  (list
                   (make-define
                    (named-let-name exp)
                    (make-lambda vars body))
                   (cons (named-let-name exp) initials))))))
             
             (cons (make-lambda vars body) initials))))

     (define (let*? exp)
       (tagged-list? exp 'let*))
     (define (let*-definitions exp)
       (cadr exp))
     (define (let*-actions exp)
       (cddr exp))

     (define (letrec? exp)
       (tagged-list? exp 'letrec))

     (define (letrec-definitions exp)
       (cadr exp))

     (define (letrec-body exp)
       (cddr exp))

     (define (letrec->let exp)
       (let* ([definitions (letrec-definitions exp)]
              [vars (map car definitions)]
              [initials (map (lambda (var) (list var '*UNASSIGNED-VALUE*)) vars)]
              [values (map cadr definitions)]
              [body (letrec-body exp)]
              [assignments (map2 make-assignment vars values)])
         (make-let
          initials
          (append
           assignments
           body))))

     (define (cond? exp) 
       (tagged-list? exp 'cond))
     (define (cond-clauses exp) (cdr exp))
     (define (cond-else-clause? clause)
       (eq? (cond-predicate clause) 'else))
     (define (cond-map-clause? clause)
       (eq? (cadr clause) '=>))

     (define (cond-predicate clause) 
       (car clause))

     (define (cond-actions clause)
       (if (cond-map-clause? clause)
           (list (list (cond-mapping-function clause) (cond-predicate clause)))
           (cdr clause)))

     (define (cond-mapping-function clause)
       (if (not (cond-map-clause? clause))
           (error "Not a cond mapping")
           (caddr clause)))

     (define (cond->if exp)
       (expand-clauses (cond-clauses exp)))
     (define (expand-clauses clauses)
       (if (null? clauses)
           'false     ; no else clause
           (let ((first (car clauses))
                 (rest (cdr clauses)))
             (if (cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp 
                      (cond-actions first))
                     (error "ELSE clause isn't 
                        last: COND->IF"
                            clauses))
                 (make-if (cond-predicate first)
                          (sequence->exp 
                           (cond-actions first))
                          (expand-clauses 
                           rest))))))

     (define (true? x)
       (not (eq? x false)))

     (define (false? x)
       (eq? x false))

     (define (and? exp)
       (tagged-list? exp 'and))

     (define (and-conditions exp) (cdr exp))

     (define (and->if exp)
       (define (helper conditions)
         (if (null? conditions)
             'true
             (make-if (car conditions) (helper (cdr conditions)) 'false)))
       (helper (and-conditions exp)))

     (define (or? exp) (tagged-list? exp 'or))
     (define (or-conditions exp) (cdr exp))

     (define (or->if exp)
       (define (helper conditions)
         (if (null? conditions)
             'false
             (make-if (car conditions) 'true (helper (cdr conditions)))))
       (helper (or-conditions exp)))

     (define (while? exp)
       (tagged-list? exp 'while))

     (define (while-cond exp)
       (cadr exp))
     (define (while-body exp)
       (cddr exp))

     (define (while->exp exp)
       (make-let
        '()
        (list
         (make-define
          'w
          (make-lambda
           '()
           (list
            (make-if
             (while-cond exp)
             (make-begin
              (append
               (while-body exp)
               (list '(w))))
             '()))))
         '(w))))
     
     (define (let*? exp)
       (tagged-list? exp 'let*))
     (define (let*-definitions exp)
       (cadr exp))
     (define (let*-actions exp)
       (cddr exp))

     (define (let*->nested-lets exp)
       (define (helper definitions)
         (let ([var (caar definitions)]
               [val (cadar definitions)])
           (if (null? (cdr definitions))
               (let* ([body (let*-actions exp)]
                      [lamb (make-lambda (list var) body)])
                 (list lamb val))
               (let ([lamb (make-lambda (list var) (list (helper (cdr definitions))))])
                 (list lamb val)))))
       (let ([definitions (let*-definitions exp)])
         (if (null? definitions)
             (make-let '() (let*-actions exp))
             (helper definitions))))

     (define (make-unbound!? exp)
       (tagged-list? exp 'make-unbound!))

     (define (make-unbound!-var exp)
       (cadr exp))

     (define (eval-make-unbound! exp env)
       (let* ([frame (first-frame env)]
              [var (make-unbound!-var exp)]
              [pair (scan-frame var frame)])
         (if (not pair)
             (error "make-unbound!: not found" var)
             (begin
               (set-frame-pairs!
                frame
                (filter
                 (lambda (pair) (not (eq? (pair-var pair) var)))
                 (frame-pairs frame)))
               'ok))))

     (define (make-procedure parameters body env)
       (list 'procedure parameters body env))

     (define (compound-procedure? p)
       (tagged-list? p 'procedure))
     (define (procedure-parameters p) (cadr p))
     (define (procedure-body p) (caddr p))
     (define (procedure-environment p) (cadddr p))

     (define (enclosing-environment env) (cdr env))
     (define (first-frame env) (car env))
     (define the-empty-environment '())

     (define (make-frame pairs)
       (mcons pairs '()))

     (define (set-frame-pairs! frame pairs)
       (set-mcar! frame pairs))

     (define (frame-pairs frame) (mcar frame))

     (define (make-pair var value) (mcons var value))
     (define pair-var mcar)
     (define pair-value mcdr)
     (define (set-pair-value! pair value)
       (set-mcdr! pair value))

     

     (define (add-binding-to-frame! var value frame)
       (set-frame-pairs! frame (cons (make-pair var value) (frame-pairs frame))))

     (define (extend-environment vars vals base-env)
       (if (= (length vars) (length vals))
           (cons (make-frame (map2 make-pair vars vals)) base-env)
           (if (< (length vars) (length vals))
               (error "Too many arguments supplied" 
                      vars 
                      vals)
               (error "Too few arguments supplied" 
                      vars 
                      vals))))

     (define (scan-frame name frame)
       (let ([pairs (frame-pairs frame)])
         (findf (lambda (pair) (eq? name (mcar pair))) pairs)))

     (define (scan-env name env)
       (if (eq? env the-empty-environment)
           false
           (let* ([frame (first-frame env)]
                  [result (scan-frame name frame)])
             (if result
                 result
                 (scan-env name (enclosing-environment env))))))

     (define (lookup-variable-value var env)
       (let ([result (scan-env var env)])
         (cond [(not result) (error "Unbound variable" var)]
               [(eq? (pair-value result) '*UNASSIGNED-VALUE*) (error "Use of unassigned variable" (pair-var result))]
               [else (pair-value result)])))

     (define (set-variable-value! var val env)
       (let ([result (scan-env var env)])
         (if (not result)
             (error "Unbound variable: SET!" var)
             (set-pair-value! result val))))

     (define (define-variable! var val env)
       (let* ([frame (first-frame env)]
              [existing-pair (scan-frame var frame)])
         (if (not existing-pair)
             (add-binding-to-frame! var val frame)
             (set-pair-value! existing-pair val))))

     (define (setup-environment)
       (let ((initial-env
              (extend-environment 
               (primitive-procedure-names)
               (primitive-procedure-objects)
               the-empty-environment)))
         (define-variable! 'true true initial-env)
         (define-variable! 'false false initial-env)
         initial-env))

     (define (primitive-procedure? proc)
       (tagged-list? proc 'primitive-metacircular))

     (define (primitive-implementation proc) 
       (cadr proc))

     (define primitive-procedures
       (list (list 'car car)
             (list 'cdr cdr)
             (list 'cons cons)
             (list 'null? null?)
             (list 'display display)
             (list '= =)
             (list '+ +)
             (list 'newline newline)
             (list '- -)))

     (define (primitive-procedure-names)
       (map car primitive-procedures))

     (define (primitive-procedure-objects)
       (map (lambda (proc) 
              (list 'primitive-metacircular (cadr proc)))
            primitive-procedures))

     (define (apply-primitive-procedure proc args)
       (apply
        (primitive-implementation proc) args))

     (define input-prompt  ";;; M-Eval input:")
     (define output-prompt ";;; M-Eval value:")


     (define the-global-environment 
       (setup-environment))
     ;    (define the-global-environment false)

     (define (driver-loop)
       (prompt-for-input input-prompt)
       (let ((input (read)))
         (let ((ms-initial (current-inexact-milliseconds)))
           (let ((output 
                  (eval-analyzed input 
                                 the-global-environment)))
             (display "Took ")
             (display (- (current-inexact-milliseconds) ms-initial))
             (display "ms")
             (newline)
             (announce-output output-prompt)
             (user-print output))))
       (driver-loop))

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

     (define (analyze exp)
       (cond ((self-evaluating? exp)
              (analyze-self-evaluating exp))
             ((quoted? exp) 
              (analyze-quoted exp))
             ((variable? exp) 
              (analyze-variable exp))
             ((assignment? exp) 
              (analyze-assignment exp))
             ((definition? exp) 
              (analyze-definition exp))
             ((and? exp)
              (analyze (and->)))
             ((if? exp) 
              (analyze-if exp))
             ((lambda? exp) 
              (analyze-lambda exp))
             ((begin? exp) 
              (analyze-sequence 
               (begin-actions exp)))
             ((let*? exp)
              (analyze (let*->nested-lets exp)))
             ((let? exp)
              (analyze-let exp))
             ((cond? exp) 
              (analyze (cond->if exp)))
             ((application? exp) 
              (analyze-application exp))
             (else
              (error "Unknown expression 
                 type: ANALYZE" 
                     exp))))

     (define (analyze-self-evaluating exp)
       (lambda (env) exp))

     (define (analyze-quoted exp)
       (let ((qval (text-of-quotation exp)))
         (lambda (env)
           qval)))

     (define (analyze-variable exp)
       (lambda (env) 
         (lookup-variable-value exp env)))

     (define (analyze-assignment exp)
       (let ((var (assignment-variable exp))
             (vproc (analyze 
                     (assignment-value exp))))
         (lambda (env)
           (set-variable-value! 
            var (vproc env) env)
           'ok)))

     (define (analyze-definition exp)
       (let ((var (definition-variable exp))
             (vproc (analyze 
                     (definition-value exp))))
         (lambda (env)
           (define-variable! var (vproc env) env)
           'ok)))

     (define (analyze-if exp)
       (let ((pproc (analyze (if-predicate exp)))
             (cproc (analyze (if-consequent exp)))
             (aproc (analyze (if-alternative exp))))
         (lambda (env)
           (if (true? (pproc env))
               (cproc env)
               (aproc env)))))

     (define (analyze-let exp)
       (let* ([combination-proc (analyze (let->combination exp))])
         (lambda (env)
           (combination-proc env))))

     (define (analyze-lambda exp)
       (let ((vars (lambda-parameters exp))
             (bproc (analyze-sequence 
                     (scan-out-defines (lambda-body exp)))))
         (lambda (env)
           (make-procedure vars bproc env))))

     (define (analyze-sequence exps)
       (define (sequentially proc1 proc2)
         (lambda (env) (proc1 env) (proc2 env)))
       (define (loop first-proc rest-procs)
         (if (null? rest-procs)
             first-proc
             (loop (sequentially first-proc 
                                 (car rest-procs))
                   (cdr rest-procs))))
       (let ((procs (map analyze exps)))
         (if (null? procs)
             (error "Empty sequence: ANALYZE")
             'nothing)
         
         (loop (car procs) (cdr procs))))

     (define (analyze-application exp)
       (let ((fproc (analyze (operator exp)))
             (aprocs (map analyze (operands exp))))
         (lambda (env)
           (execute-application 
            (fproc env)
            (map (lambda (aproc) (aproc env))
                 aprocs)))))

     (define (execute-application proc args)
       (cond ((primitive-procedure? proc)
              (apply-primitive-procedure proc args))
             ((compound-procedure? proc)
              (begin
                ((procedure-body proc)
                 (extend-environment 
                  (procedure-parameters proc)
                  args
                  (procedure-environment proc)))))
             (else (error "Unknown procedure type: 
                      EXECUTE-APPLICATION"
                          proc))))

     (define (eval-analyzed exp env)
       ((analyze exp) env))

     (define initial-env
       '(begin
          (define (fib n)
            (define (f m k0 k1)
              (if (= m n)
                  k0
                  (f (+ m 1) k1 (+ k0 k1))))
            (f 0 0 1))))
     
     (eval-analyzed initial-env the-global-environment)
     ;     (eval-analyzed '(define x 2) the-global-environment)
     (driver-loop)
     ))

;; (define ns (make-base-namespace))
;; (eval '(define true #t) ns)
;; (eval '(define false #f) ns)
;; (eval metacircular ns)
;; (eval '(driver-loop) ns)


