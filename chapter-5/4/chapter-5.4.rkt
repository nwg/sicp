#lang racket

(require "../2/base.rkt")
(require compatibility/mlist)
(require "../../utility.rkt")

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (mtagged-list? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))

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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

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

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

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

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
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

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-definitions exp)
  (cadr exp))

(define (let-actions exp)
      (cddr exp))

(define (let->combination exp)
  (let* ([definitions (let-definitions exp)]
         [vars (map car definitions)]
         [initials (map cadr definitions)]
         [body (let-actions exp)])
    (cons (make-lambda vars body) initials)))

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

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (user-print object)
  (if (compound-procedure? object)
      (displayln 
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (displayln object)))

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

(define (last-operand? ops) (null? (cdr ops)))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))

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

(define (delay-it exp env)
  (mcons 'thunk (mcons exp (mcons env '()))))
(define (thunk? obj) (mtagged-list? obj 'thunk))
(define (thunk-exp thunk) (mcadr thunk))
(define (thunk-env thunk) (mcaddr thunk))

(define (evaluated-thunk? obj)
  (mtagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) 
  (mcadr evaluated-thunk))

(define (set-thunk-evaluated! thunk value)
  (if (not (thunk? thunk))
      (error "Not a thunk")
      (begin
        (set-mcar! thunk 'evaluated-thunk)
        ;; replace exp with its value:
        (set-mcar! (mcdr thunk) value) 
        ;; forget unneeded env:
        (set-mcdr! (mcdr thunk) '())
        'ok))) 

(define the-global-environment
  (setup-environment))

(define (get-global-environment)
  the-global-environment)

(define eceval-operations
  (list (list 'self-evaluating? 
              self-evaluating?)
        (list 'thunk? thunk?)
        (list 'evaluated-thunk? evaluated-thunk?)
        (list 'set-thunk-evaluated! set-thunk-evaluated!)
        (list 'thunk-exp thunk-exp)
        (list 'thunk-env thunk-env)
        (list 'thunk-value thunk-value)
        (list 'delay-it delay-it)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'adjoin-arg adjoin-arg)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'cond->if cond->if)
        (list 'cond? cond?)
        (list 'let? let?)
        (list 'let->combination let->combination)
        (list 'or? or?)
        (list 'or->if or->if)
        (list 'and? and?)
        (list 'and->if and->if)
        ))

(define eceval
  (make-machine
   eceval-operations
   '(read-eval-print-loop
       (perform (op initialize-stack))
       (perform (op prompt-for-input)
                (const ";;; EC-Eval input:"))
       (assign exp (op read))
       (assign env (op get-global-environment))
       (assign continue (label print-result))
       (goto (label actual-value))
     print-result
       (perform (op announce-output)
                (const ";;; EC-Eval value:"))
       (perform (op user-print) (reg val))
       (goto (label read-eval-print-loop))
     unknown-expression-type
       (assign 
        val
        (const unknown-expression-type-error))
       (goto (label signal-error))
     unknown-procedure-type
       ; clean up stack (from apply-dispatch):
       (restore continue)    
       (assign 
        val
        (const unknown-procedure-type-error))
       (goto (label signal-error))
     signal-error
       (perform (op user-print) (reg val))
       (goto (label read-eval-print-loop))

     eval-dispatch
       (test (op self-evaluating?) (reg exp))
       (branch (label ev-self-eval))
       (test (op variable?) (reg exp))
       (branch (label ev-variable))
       (test (op quoted?) (reg exp))
       (branch (label ev-quoted))
       (test (op assignment?) (reg exp))
       (branch (label ev-assignment))
       (test (op definition?) (reg exp))
       (branch (label ev-definition))
       (test (op if?) (reg exp))
       (branch (label ev-if))
       (test (op cond?) (reg exp))
       (branch (label ev-cond))
       (test (op let?) (reg exp))
       (branch (label ev-let))
       (test (op and?) (reg exp))
       (branch (label ev-and))
       (test (op or?) (reg exp))
       (branch (label ev-or))
       (test (op lambda?) (reg exp))
       (branch (label ev-lambda))
       (test (op begin?) (reg exp))
       (branch (label ev-begin))
       (test (op application?) (reg exp))
       (branch (label ev-application))
       (goto (label unknown-expression-type))

     actual-value
       (save continue)
       (assign continue (label av-after-eval))
       (goto (label eval-dispatch))
     av-after-eval
       (restore continue)
       (goto (label force-it))
       
     force-it
       (test (op thunk?) (reg val))
       (branch (label is-thunk))
       (test (op evaluated-thunk?) (reg val))
       (branch (label is-evaluated-thunk))
       (goto (reg continue))
       
     is-thunk
       (assign exp (op thunk-exp) (reg val))
       (assign env (op thunk-env) (reg val))
       (save continue)
       (assign continue (label fi-is-thunk-after-av))
       (save val)
       (goto (label actual-value))
     fi-is-thunk-after-av
       (restore exp) ; old val
       (perform (op set-thunk-evaluated!) (reg exp) (reg val))
       (restore continue)
       (goto (reg continue))

     is-evaluated-thunk
       (assign val (op thunk-value) (reg val))
       (goto (reg continue))
       
     ev-self-eval
       (assign val (reg exp))
       (goto (reg continue))
     ev-variable
       (assign val
               (op lookup-variable-value)
               (reg exp)
               (reg env))
       (goto (reg continue))
     ev-quoted
       (assign val
               (op text-of-quotation)
               (reg exp))
       (goto (reg continue))
     ev-lambda
       (assign unev
               (op lambda-parameters)
               (reg exp))
       (assign exp 
               (op lambda-body)
               (reg exp))
       (assign val 
               (op make-procedure)
               (reg unev)
               (reg exp)
               (reg env))
       (goto (reg continue))

     ev-application
       (save continue)
       (save env)
       (assign unev (op operands) (reg exp))
       (save unev)
       (assign exp (op operator) (reg exp))
       (assign
        continue (label ev-appl-did-operator))
       (goto (label eval-dispatch))

     ev-appl-did-operator
       (restore unev)             ; the operands
       (restore env)
       (assign argl (op empty-arglist))
       (assign proc (reg val))    ; the operator

       (test (op primitive-procedure?) (reg proc))
       (branch (label ev-appl-primitive))
       (test (op compound-procedure?) (reg proc))
       (branch (label ev-appl-compound))
       (goto (label unknown-procedure-type))

     ev-appl-compound
     ev-appl-compound-loop
       (test (op no-operands?) (reg unev))
       (branch (label compound-apply))
       (assign exp
               (op first-operand)
               (reg unev))

       (assign val (op delay-it) (reg exp) (reg env))
       (assign argl 
               (op adjoin-arg)
               (reg val)
               (reg argl))
       (assign unev
               (op rest-operands)
               (reg unev))
       (goto (label ev-appl-compound-loop))
     compound-apply
       (assign unev 
               (op procedure-parameters)
               (reg proc))
       (assign env
               (op procedure-environment)
               (reg proc))
       (assign env
               (op extend-environment)
               (reg unev)
               (reg argl)
               (reg env))
       (assign unev
               (op procedure-body)
               (reg proc))
       (goto (label ev-sequence))

       

       
     ev-appl-primitive
       (test (op no-operands?) (reg unev))
       (branch (label primitive-apply))
       (save proc)
     ev-appl-operand-loop
       (save argl)
       (assign exp
               (op first-operand)
               (reg unev))
       (test (op last-operand?) (reg unev))
       (branch (label ev-appl-last-arg))
       (save env)
       (save unev)
       (assign continue 
               (label ev-appl-accumulate-arg))
       (goto (label actual-value))

     ev-appl-accumulate-arg
       (restore unev)
       (restore env)
       (restore argl)
       (assign argl 
               (op adjoin-arg)
               (reg val)
               (reg argl))
       (assign unev
               (op rest-operands)
               (reg unev))
       (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
       (assign continue 
               (label ev-appl-accum-last-arg))
       (goto (label actual-value))
     ev-appl-accum-last-arg
       (restore argl)
       (assign argl 
               (op adjoin-arg)
               (reg val)
               (reg argl))
       (restore proc)
       (goto (label primitive-apply))

     primitive-apply
       (assign val (op apply-primitive-procedure)
               (reg proc)
               (reg argl))
       (restore continue)
       (goto (reg continue))

     ev-begin
       (assign unev
               (op begin-actions)
               (reg exp))
       (save continue)
       (goto (label ev-sequence))

     ev-sequence
       (assign exp (op first-exp) (reg unev))
       (test (op last-exp?) (reg unev))
       (branch (label ev-sequence-last-exp))
       (save unev)
       (save env)
       (assign continue
               (label ev-sequence-continue))
       (goto (label eval-dispatch))
     ev-sequence-continue
       (restore env)
       (restore unev)
       (assign unev
               (op rest-exps)
               (reg unev))
       (goto (label ev-sequence))
     ev-sequence-last-exp
       (restore continue)
       (goto (label eval-dispatch))

     ev-if
       (save exp)   ; save expression for later
       (save env)
       (save continue)
       (assign continue (label ev-if-decide))
       (assign exp (op if-predicate) (reg exp))
       ; evaluate the predicate:
       (goto (label actual-value))

     ev-if-decide
       (restore continue)
       (restore env)
       (restore exp)
       (test (op true?) (reg val))
       (branch (label ev-if-consequent))
     ev-if-alternative
       (assign exp (op if-alternative) (reg exp))
       (goto (label eval-dispatch))
     ev-if-consequent
       (assign exp (op if-consequent) (reg exp))
       (goto (label eval-dispatch))
     ev-cond
       (assign exp (op cond->if) (reg exp))
       (goto (label eval-dispatch))
     ev-let
       (assign exp (op let->combination) (reg exp))
       (goto (label eval-dispatch))
     ev-and
       (assign exp (op and->if) (reg exp))
       (goto (label eval-dispatch))
     ev-or
       (assign exp (op or->if) (reg exp))
       (goto (label eval-dispatch))
  
     ev-assignment
       (assign unev 
               (op assignment-variable)
               (reg exp))
       (save unev)   ; save variable for later
       (assign exp
               (op assignment-value)
               (reg exp))
       (save env)
       (save continue)
       (assign continue
               (label ev-assignment-1))
       ; evaluate the assignment value:
       (goto (label eval-dispatch))  
     ev-assignment-1
       (restore continue)
       (restore env)
       (restore unev)
       (perform (op set-variable-value!)
                (reg unev)
                (reg val)
                (reg env))
       (assign val
               (const ok))
       (goto (reg continue))

     ev-definition
       (assign unev 
               (op definition-variable)
               (reg exp))
       (save unev)   ; save variable for later
       (assign exp 
               (op definition-value)
               (reg exp))
       (save env)
       (save continue)
       (assign continue (label ev-definition-1))
       ; evaluate the definition value:
       (goto (label eval-dispatch))  
     ev-definition-1
       (restore continue)
       (restore env)
       (restore unev)
       (perform (op define-variable!)
                (reg unev)
                (reg val)
                (reg env))
       (assign val (const ok))
       (goto (reg continue))
       )))


;(eceval 'trace-on)
(start eceval)