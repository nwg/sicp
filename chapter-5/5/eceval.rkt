#lang racket

(require "machine.rkt")
(require "interpreter.rkt")
(require "syntax.rkt")
(require "../../utility.rkt")
(require "compiler.rkt")
(provide eceval)

(define (interpreter-compile expression)
  (let-values ([(instructions labels) (assemble (statements (compile expression 'val 'return the-empty-compile-environment)) eceval)])
    instructions))

(define eceval-operations
  (list (list 'compile interpreter-compile)
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


(define eceval
  (make-machine
   eceval-operations
   '(  (branch (label external-entry)) 
     
     read-compile-print-loop
       (perform (op initialize-stack))
       (perform (op prompt-for-input)
                (const ";;; EC-Eval input:"))
       (assign exp (op read))
       (assign val (op compile) (reg exp))
       (assign env (op get-global-environment))
       (assign continue (label print-result))
       (goto (reg val))

     print-result
       (perform (op print-stack-statistics))
       (perform (op announce-output)
                (const ";;; EC-Eval value:"))
       (perform (op user-print) (reg val))
       (goto (label read-compile-print-loop))
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
       (goto (label read-compile-print-loop))
     external-entry
       (perform (op initialize-stack))
       (assign env (op get-global-environment))
       (assign continue (label print-result))
       (goto (reg val))


       )))

