#lang racket

(require "machine.rkt")
(require "syntax.rkt")
(require "../../utility.rkt")
(require "compiler.rkt")
(require compatibility/mlist)
(require "eceval-objects.rkt")
(require racket/pretty)

(provide make-eceval)
(provide setup-environment)

(define (add-binding-to-frame! var val frame)
  (set-mcar! (frame-pairs frame) (cons var (mcar (frame-pairs frame))))
  (set-mcdr! (frame-pairs frame) (mcons val (mcdr (frame-pairs frame)))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars (length vals))
          (error "Too few arguments supplied" 
                 vars))))

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
        (list 'equal? equal?)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'mcar mcar)
        (list 'mcdr mcdr)
        (list 'mcons mcons)
        (list 'set-mcdr! set-mcdr!)
        (list 'list list)
        (list 'newline newline)
        (list 'display display)
        (list 'displayln displayln)
        (list 'append append)
        (list '= =)
        (list '* *)
        (list '- -)
        (list '> >)
        (list '< <)
        (list '+ +)
        (list 'length length)
        (list 'eq? eq?)
        (list 'apply-racket apply)
        (list 'current-inexact-milliseconds current-inexact-milliseconds)
        (list 'read read)
        (list 'number? number?)
        (list 'symbol? symbol?)
        (list 'pair? pair?)
        (list 'string? string?)
        (list 'not not)
        (list 'error error)
        (list 'cddr cddr)
        (list 'cdadr cdadr)
        (list 'cdddr cdddr)
        (list 'cadddr cadddr)
        (list 'caddr caddr)
        (list 'caadr caadr)
        (list 'cadar cadar)
        (list 'caar caar)
        (list 'reverse reverse)
        (list 'set-mcar! set-mcar!)
        (list 'pretty-print pretty-print)))

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

(define (compile-and-run? exp)
  (tagged-list? exp 'compile-and-run))

(define (compile-and-run-expression exp) (cadadr exp))

(define (interpreter-compile expression machine)
  (let-values ([(instructions labels) (assemble (statements (compile expression 'val 'return the-empty-compile-environment)) machine)])
    instructions))

(define (eceval-operations machine)
  (define (compile-operation exp)
    (interpreter-compile exp machine))

  (list (list 'compile compile-operation)
        (list 'lexical-address-set! lexical-address-set!)
        (list 'compile-and-run? compile-and-run?)
        (list 'compile-and-run-expression compile-and-run-expression)
        (list 'displayln displayln)
        (list 'display display)
        (list 'cons cons)
        (list '= =)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list 'false? false?)
        (list 'list list)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'lexical-address-lookup lexical-address-lookup)
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'self-evaluating? 
              self-evaluating?)
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
        (list 'application-simple? application-simple?)
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

(define eceval-insts
  '(     (assign compapp (label compound-apply))
         (assign app (label apply-dispatch))
         (branch (label external-entry)) 
         
         read-eval-print-loop
         (perform (op initialize-stack))
         (perform (op prompt-for-input)
                  (const ";;; EC-Eval input:"))
         (assign exp (op read))
         (assign env (op get-global-environment))
         (assign continue (label print-result))
         (goto (label eval-dispatch))

         print-result
         (perform (op print-stack-statistics))
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
         (test (op compile-and-run?) (reg exp))
         (branch (label ev-compile-and-run))
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
         (test (op application-simple?) (reg exp))
         (branch (label ev-application-simple))
         (test (op application?) (reg exp))
         (branch (label ev-application))
         (goto (label unknown-expression-type))

         external-entry
         (perform (op initialize-stack))
         (assign env (op get-global-environment))
         (assign continue (label print-result))
         (goto (reg val))

         ev-compile-and-run
         (assign exp (op compile-and-run-expression) (reg exp))
         (assign val (op compile) (reg exp))
         (goto (reg val))
         
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

         ev-application-simple
         ;(perform (op user-print) (const "in simple"))
         (save continue)
         (assign unev (op operands) (reg exp))
         (assign exp (op operator) (reg exp))
         (assign continue (label ev-appl-simple-did-operator))
         (goto (label eval-dispatch))
         ev-appl-simple-did-operator
         (assign proc (reg val))
         (goto (label ev-appl-operands))
         
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
         (assign proc (reg val))    ; the operator
         ev-appl-operands
         (assign argl (op empty-arglist))
         (test (op no-operands?) (reg unev))
         (branch (label apply-dispatch))
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
         (goto (label eval-dispatch))

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
         (goto (label eval-dispatch))
         ev-appl-accum-last-arg
         (restore argl)
         (assign argl 
                 (op adjoin-arg)
                 (reg val)
                 (reg argl))
         (restore proc)
         (goto (label apply-dispatch))

         apply-dispatch
         (test (op primitive-procedure?) (reg proc))
         (branch (label primitive-apply))
         (test (op compound-procedure?) (reg proc))
         (branch (label compound-apply))
         (test (op compiled-procedure?) (reg proc))
         (branch (label compiled-apply))       
         (goto (label unknown-procedure-type))

         primitive-apply
         (assign val (op apply-primitive-procedure)
                 (reg proc)
                 (reg argl))
         (restore continue)
         (goto (reg continue))

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
         
         compiled-apply
         (restore continue)
         (assign val
                 (op compiled-procedure-entry)
                 (reg proc))
         (goto (reg val))
         
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
         (goto (label eval-dispatch))

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
         ))


(define (make-eceval)
    (let* ([trace-file (open-output-file "eceval.trace" #:mode 'text #:exists 'truncate)]
           [eceval (make-new-machine trace-file)])

      (install-operations eceval (eceval-operations eceval))
      (install-instructions eceval eceval-insts)
      eceval)

  )
