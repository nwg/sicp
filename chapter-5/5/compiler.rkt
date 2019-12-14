#lang racket

(require "syntax.rkt")
(require (only-in "../../utility.rkt" tagged-list?))

(provide compile)
(provide statements)
(provide the-empty-compile-environment)

(define all-regs '(env proc val argl continue arg1 arg2))

(define label-counter 0)

(define the-empty-compile-environment '())
(define (extend-compile-environment vars env) (cons vars env))
(define (enclosing-compile-environment env) (cdr env))

(define (find-variable var env)
  (define (find-variable-internal env-pos var env)
    (if (eq? env the-empty-compile-environment)
        'not-found
        (let ([var-pos (index-of (car env) var)])
          (if var-pos
              (list env-pos var-pos)
              (find-variable-internal (+ env-pos 1) var (cdr env))))))
  (find-variable-internal 0 var env))

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append 
    (symbol->string name)
    (number->string (new-label-number)))))


(define (open-coded-operator exp)
  (car exp))

(define (open-coded? exp env)
  (and
   (or
    (tagged-list? exp '=)
    (tagged-list? exp '+)
    (tagged-list? exp '*)
    (tagged-list? exp '-))
   (eq? (find-variable (car exp) env) 'not-found)))

(define (compile exp target linkage env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating 
          exp target linkage))
        ((quoted? exp) 
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable 
          exp target linkage env))
        ((assignment? exp)
         (compile-assignment
          exp target linkage env))
        ((definition? exp)
         (compile-definition
          exp target linkage env))
        ((if? exp)
         (compile-if exp target linkage env))
        ((lambda? exp)
         (compile-lambda exp target linkage env))
        ((let? exp)
         (compile (let->combination exp) target linkage env))
        ((begin? exp)
         (compile-sequence 
          (begin-actions exp) target linkage env))
        ((cond? exp) 
         (compile 
          (cond->if exp) target linkage env))
        ((open-coded? exp env)
         (compile-open-coded (open-coded-operator exp) exp target linkage env))
        ((application? exp)
         (compile-application 
          exp target linkage env))
        (else
         (error "Unknown expression type: 
                 COMPILE" 
                exp))))

(define (make-instruction-sequence 
         needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence 
          '(continue)
          '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage 
         linkage instruction-sequence)
  (preserving
   '(continue)
   instruction-sequence
   (compile-linkage linkage)))

(define (compile-self-evaluating 
         exp target linkage)
  (end-with-linkage
   linkage (make-instruction-sequence 
            '()
            (list target)
            `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign 
       ,target
       (const ,(text-of-quotation exp)))))))

(define (compile-variable
         exp target linkage env)
  (let ([var-address (find-variable exp env)])
    (end-with-linkage 
     linkage
     (if (eq? var-address 'not-found)
         (make-instruction-sequence
          '()
          `(,target env)
          `((assign env (op get-global-environment))
            (assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env))))
         (make-instruction-sequence 
          '(env)
          (list target)
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,var-address)
                    (reg env))))))))

(define (compile-assignment 
         exp target linkage env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 
                  'val
                  'next
                  env)))
    (let ([var-address (find-variable var env)])
      (end-with-linkage 
       linkage
       (preserving 
        '(env)
        get-value-code
        (if (eq? var-address 'not-found)
            (make-instruction-sequence
             '(val)
             `(,target env)
             `((assign env (op get-global-environment))
               (perform (op set-variable-value!)
                        (const ,var)
                        (reg val)
                        (reg env))))
            (make-instruction-sequence
             '(env val)
             (list target)
             `((perform (op lexical-address-set!)
                        (const ,var-address)
                        (reg val)
                        (reg env))
               (assign ,target (const ok))))))))))

(define (compile-definition 
         exp target linkage env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp)
                  'val
                  'next
                  env)))
    (end-with-linkage
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-if exp target linkage env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) 
               after-if
               linkage)))
      (let ((p-code 
             (compile (if-predicate exp)
                      'val
                      'next
                      env))
            (c-code
             (compile (if-consequent exp) 
                      target 
                      consequent-linkage
                      env))
            (a-code
             (compile (if-alternative exp)
                      target
                      linkage
                      env)))
        (preserving 
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence 
           '(val) 
           '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences 
            t-branch c-code)
           (append-instruction-sequences
            f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage env)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next env)
       (compile-sequence (rest-exps seq)
                         target
                         linkage
                         env))))

(define (compile-lambda exp target linkage environment)
  (let ((proc-entry 
         (make-label 'entry))
        (after-lambda 
         (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next)
               after-lambda
               linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage 
         lambda-linkage
         (make-instruction-sequence 
          '(env)
          (list target)
          `((assign 
             ,target
             (op make-compiled-procedure)
             (label ,proc-entry)
             (reg env)))))
        (compile-lambda-body exp proc-entry environment))
       after-lambda))))

(define (compile-lambda-body exp proc-entry environment)
  (let* ((formals (lambda-parameters exp))
         (extended-compile-env (extend-compile-environment formals environment)))
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env 
                (op compiled-procedure-env)
                (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp))
                       'val
                       'return
                       extended-compile-env))))

(define (open-coded-rest-ops op operands target env)
  (if (null? operands)
      (empty-instruction-sequence)
      (let ([dest (if (null? (cdr operands))
                      target
                      'arg1)])
        (preserving
         '(arg1 env)
         (compile (car operands) 'arg2 'next env)
         (append-instruction-sequences          
          (make-instruction-sequence
           '(arg1 arg2)
           (list dest)
           `((assign ,target (op ,op) (reg arg1) (reg arg2))))
          (open-coded-rest-ops op (cdr operands) target env))))))

(define (compile-open-coded op exp target linkage env)
  (let* ([operandss (operands exp)]
         [arg1-compiled (compile (car operandss) 'arg1 'next env)])
    (end-with-linkage
     linkage
     (preserving
      '(env)
      arg1-compiled
      (open-coded-rest-ops op (cdr operandss) target env)))))    
    
(define (compile-application 
         exp target linkage env)
  (let ((proc-code 
         (compile (operator exp) 'proc 'next env))
        (operand-codes
         (map (lambda (operand)
                (compile operand 'val 'next env))
              (operands exp))))
    (preserving 
     '(env continue)
     proc-code
     (preserving 
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call 
       target
       linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes 
         (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence 
         '() 
         '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence 
                 '(val)
                 '(argl)
                 '((assign argl
                           (op list)
                           (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving 
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving 
          '(argl)
          (car operand-codes)
          (make-instruction-sequence 
           '(val argl)
           '(argl)
           '((assign argl
                     (op cons)
                     (reg val)
                     (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving 
         '(env)
         code-for-next-arg
         (code-to-get-rest-args 
          (cdr operand-codes))))))

(define (compile-procedure-call
         target linkage)
  (let ((primitive-branch 
         (make-label 'primitive-branch))
        (compiled-branch 
         (make-label 'compiled-branch))
        (compound-branch
         (make-label 'compound-branch))
        (after-call
         (make-label 'after-call)))
    (let ((branch-linkage
           (if (eq? linkage 'next)
               after-call
               linkage)))
      (append-instruction-sequences
       (make-instruction-sequence 
        '(proc)
        '()
        `((test 
           (op primitive-procedure?)
           (reg proc))
          (branch 
           (label ,primitive-branch))
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl 
          target
          branch-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl)
           (list target)
           `((assign 
              ,target
              (op apply-primitive-procedure)
              (reg proc)
              (reg argl))))))
        (append-instruction-sequences
         compound-branch
         (compile-interp-proc-appl target branch-linkage)))
       after-call))))

(define (compile-interp-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence 
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            (save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return 
                (make-label 'proc-return)))
           (make-instruction-sequence 
            '(proc)
            all-regs
            `((assign continue 
                      (label ,proc-return))
              (save continue)
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence 
          '(proc continue) 
          all-regs
          '((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, 
                 target not val: COMPILE"
                target))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence 
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            (assign 
             val 
             (op compiled-procedure-entry)
             (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return 
                (make-label 'proc-return)))
           (make-instruction-sequence 
            '(proc)
            all-regs
            `((assign continue 
                      (label ,proc-return))
              (assign 
               val 
               (op compiled-procedure-entry)
               (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence 
          '(proc continue) 
          all-regs
          '((assign 
             val 
             (op compiled-procedure-entry)
             (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, 
                 target not val: COMPILE"
                target))))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union 
      (registers-needed seq1)
      (list-difference 
       (registers-needed seq2)
       (registers-modified seq1)))
     (list-union
      (registers-modified seq1)
      (registers-modified seq2))
     (append (statements seq1)
             (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences 
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2)
         (list-union (cdr s1) s2))
        (else
         (cons (car s1)
               (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else 
         (cons (car s1)
               (list-difference (cdr s1)
                                s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and 
             (needs-register? seq2 first-reg)
             (modifies-register? seq1 
                                 first-reg))
            (preserving 
             (cdr regs)
             (make-instruction-sequence
              (list-union 
               (list first-reg)
               (registers-needed seq1))
              (list-difference
               (registers-modified seq1)
               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving 
             (cdr regs)
             seq1
             seq2)))))

(define (tack-on-instruction-sequence 
         seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))


(define (parallel-instruction-sequences . seqs)
  (foldr parallel-instruction-sequences2 (empty-instruction-sequence) seqs))

(define (parallel-instruction-sequences2
         seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))

;; (compile
;;  '(define (f + * a b x y)
;;     (+ (* a x) (* b y)))
;;  'val
;;  'next
;;  the-empty-compile-environment)

;; (compile
;;  '(define (f a b x y)
;;     (+ (* a x) (* b y)))
;;  'val
;;  'next
;;  the-empty-compile-environment)
