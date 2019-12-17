#lang racket

(require "../../utility.rkt")

(provide make-new-machine)
(provide install-operations)
(provide install-instructions)
(provide set-register-contents!)
(provide get-register-contents)
(provide start)
(provide assemble)
(require "eceval-objects.rkt")

(define (install-operations machine ops)
  ((machine 'install-operations) ops))

(define (install-instructions machine insts)
    (let-values ([(seq labels) (assemble insts machine)])
      ((machine 'install-instruction-sequence) seq labels)))

(define (insts-string seq)
  (cond [(null? seq) "<no instructions>"]
        [(not (null? (instruction-labels (car seq))))
         (string-join (map ~s (instruction-labels (car seq))) "/")]
        [else (string-append
               (car (instruction-text (car seq)))
               "...")]))

(define (user-string val)
  (cond [(pair? val)
         (cond [(instruction? (car val)) (insts-string val)]
               [(compiled-procedure? val)
                (string-append
                  "<compiled-procedure: "
                  (insts-string (compiled-procedure-entry val))
                  ">")]
               [(environment? val) (display-string-for-environment val)]
               [else (~s val)])]
        [else (~s val)]))

(define (make-register name trace-file)
  (let ((contents *UNASSIGNED-VALUE*)
        (trace-enabled false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (when trace-enabled
                 (display "reg " trace-file)
                 (display name trace-file)
                 (display " -- " trace-file)
                 (display (user-string contents) trace-file)
                 (display " -> " trace-file)
                 (display (user-string value) trace-file)
                 (newline trace-file))
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! trace-enabled true))
            ((eq? message 'trace-off)
             (set! trace-enabled false))
            (else
             (error "Unknown request: 
                     REGISTER"
                    message))))
    dispatch))

(define *UNASSIGNED-VALUE* '*unassigned*)

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth 
            (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth
                  (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)

    (define (print-statistics)
      (display (list 'total-pushes 
                     '= 
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth))
      (newline))
    
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))
(define (pop stack) (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine [trace-file (current-output-port)])
  (let ((pc (make-register 'pc trace-file))
        (flag (make-register 'flag trace-file))
        (stack (make-stack))
        (the-instruction-sequence '())
        (execution-count 0)
        (trace-enabled false)
        (labels '()))
    (define (get-execution-count)
      (display "instructions executed since last call: ")
      (display execution-count)
      (newline)
      (set! execution-count 0))
    (let ((the-ops
           (list 
            (list 'initialize-stack
                  (lambda () 
                    (stack 'initialize)))
            (list 'print-stack-statistics
                  (lambda () 
                    (stack 'print-statistics)))
            (list 'get-execution-count
                  (lambda ()
                    (get-execution-count)))))
          (register-table
           (list (list 'pc pc) 
                 (list 'flag flag))))
      (define (inst-at label offset)
        (let ([ref-entry (assoc label labels)])
          (if (not ref-entry)
              false
              (with-handlers ([exn:fail:contract? (λ (e) false)])
                (list-ref (cdr ref-entry) offset)))))        
      (define (set-breakpoint label offset)
        (let ([inst (inst-at label offset)])
          (if (not inst)
              (displayln "could not find instruction")              
              (set-instruction-breakpoint! inst (make-breakpoint label offset)))))
      (define (cancel-breakpoint label offset)
        (let ([inst (inst-at label offset)])
          (if (not inst)
              (displayln "could not find instruction")
              (let ([bp (instruction-breakpoint inst)])
                (if (not bp)
                    (displayln "Could not cancel breakpoint (existing breakpoint not found)")
                    (set-instruction-breakpoint! inst false))))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error 
             "Multiply defined register: " 
             name)
            (let ([register (make-register name trace-file)])
              (set! register-table
                    (cons 
                     (list name register)
                     register-table))
              register)))
      (define (lookup-register name)
        (let ((val 
               (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" 
                     name))))
      (define (get-or-create-register name)
        (let ([val (assoc name register-table)])
          (if val
              (cadr val)
              (allocate-register name))))
      (define (proceed-with-instruction inst)
        ((instruction-execution-proc inst))
        (set! execution-count (+ execution-count 1))
        (execute))
      (define (proceed-execution)
        (let ([insts (get-contents pc)])
          (cond [(null? insts) 'done]
                [(eq? (car insts) *UNASSIGNED-VALUE*)
                 (error "Machine not started")]
                [else (proceed-with-instruction (car insts))])))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (when trace-enabled
                  (for-each
                   (lambda (label)
                     (display label trace-file)
                     (display ":" trace-file)
                     (newline trace-file))
                   (instruction-labels (car insts)))
                  (displayln (instruction-text (car insts)) trace-file)
                  (flush-output trace-file)
                  )
                (let ([bp (instruction-breakpoint (car insts))])
                  (if bp
                      (begin
                        (display "breakpoint -- ")
                        (display (breakpoint-label bp))
                        (display " + ")
                        (display (breakpoint-offset bp))
                        (newline))
                      (proceed-with-instruction (car insts))))))))
                        
      (define (trace-register name)
        (let ([reg (assoc name register-table)])
          (if (not reg)
              (error "Register not found:" name)
              ((cadr reg) 'trace-on))))
      (define (stop-trace-register name)
        (let ([reg (assoc name register-table)])
          (if (not reg)
              (error "Register not found:" name)
              ((cadr reg) 'trace-off))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! 
                pc
                the-instruction-sequence)
               (execute))
              ((eq? message 'proceed)
               (proceed-execution))
              ((eq? 
                message 
                'install-instruction-sequence)
               (lambda (seq new-labels) 
                 (set! the-instruction-sequence seq)
                 (set! labels new-labels)))
              ((eq? message 
                    'allocate-register) 
               allocate-register)
              ((eq? message 'get-register) 
               lookup-register)
              ((eq? message 'get-or-create-register) 
               get-or-create-register)
              ((eq? message 
                    'install-operations)
               (lambda (ops) 
                 (set! the-ops 
                       (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) 
               the-ops)
              ((eq? message 'sorted-instructions)
               (groupby car (rm-dupes (map mcar the-instruction-sequence))))
              ((eq? message 'entry-point-regs)
               (entry-point-regs (map mcar the-instruction-sequence)))
              ((eq? message 'save-restore-regs)
               (save-restore-regs (map mcar the-instruction-sequence)))
              ((eq? message 'assignment-sources)
               (assignment-sources-grouped (map mcar the-instruction-sequence)))
              ((eq? message 'trace-on)
               (set! trace-enabled true))
              ((eq? message 'trace-off)
               (set! trace-enabled false))
              ((eq? message 'trace-register)
               trace-register)
              ((eq? message 'stop-trace-register)
               stop-trace-register)
              ((eq? message 'set-breakpoint)
               set-breakpoint)
              ((eq? message 'cancel-breakpoint)
               cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints)
               (for-each
                (lambda (inst)
                  (set-instruction-breakpoint! inst false))
                the-instruction-sequence))
              (else (error "Unknown request: 
                            MACHINE"
                           message))))
      dispatch)))

(define (set-breakpoint machine label offset)
  ((machine 'set-breakpoint) label offset))
(define (cancel-breakpoint machine label offset)
  ((machine 'cancel-breakpoint) label offset))
(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))
  
(define (assignment-sources-grouped insts)
 (map
  (lambda (pair)
    (list
     (car pair)
     (map
      (lambda (inst)
        (assign-value-exp inst))
      (cadr pair))))
  (groupby
   assign-reg-name
   (filter
    (lambda (inst) (equal? (car inst) 'assign))
    (rm-dupes insts)))))

(define (save-restore-regs insts)
  (rm-dupes
   (map
    stack-inst-reg-name
    (filter
     (lambda (inst)
       (or
        (eq? (car inst) 'save)
        (eq? (car inst) 'restore)))
     insts))))

(define (entry-point-regs insts)
  (rm-dupes
   (map
    (lambda (inst)
      (cadr (goto-dest inst)))
    (filter
     (lambda (inst)
       (and
        (eq? (car inst) 'goto)
        (register-exp? (goto-dest inst))))
     insts))))

(define (start machine)
  (machine 'start))

(define (proceed-machine machine)
  (machine 'proceed))

(define (get-register-contents 
         machine register-name)
  (get-contents 
   (get-register machine register-name)))

(define (set-register-contents! 
         machine register-name value)
  (set-contents! 
   (get-register machine register-name) 
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (get-or-create-register machine reg-name)
  ((machine 'get-or-create-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      (values insts labels))))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels 
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "Label already exists --" next-inst)
                   (receive 
                    insts
                    (cons 
                     (make-label-entry 
                      next-inst
                      insts)
                     labels)))
               (receive 
                   (cons (make-instruction 
                          next-inst)
                         insts)
                   labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) 
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)
    (for-each
     (lambda (pair)
       (when (not (null? (cdr pair)))
         (let ([label (car pair)]
               [inst (cadr pair)])
           (set-instruction-labels! inst (append (instruction-labels inst) (list label))))))
     labels)))

(define (make-breakpoint label offset)
  (cons label offset))
(define breakpoint-label car)
(define breakpoint-offset cdr)

(define (make-instruction text)
  (cons 'instruction (mcons text (mcons '() (mcons '() false)))))
(define (instruction? x) (tagged-list? x 'instruction))
(define (instruction-text inst) (mcar (data inst)))
(define (instruction-execution-proc inst)
  (mcadr (data inst)))
(define (instruction-labels inst)
  (mcaddr (data inst)))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-mcar! (mcdr (data inst)) proc))
(define (set-instruction-labels! inst labels)
  (set-mcar! (mcddr (data inst)) labels))
(define (set-instruction-breakpoint! inst b)
  (set-mcdr! (mcddr (data inst)) b))
(define (instruction-breakpoint inst)
  (mcdddr (data inst)))
(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" 
               label-name))))

(define (make-execution-procedure 
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign 
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test 
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch 
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction 
                      type: ASSEMBLE"
                     inst))))

(define (make-assign 
         inst machine labels operations pc)
  (if (not (symbol? (assign-reg-name inst)))
      (error "assignment target must be symbol, got" (assign-reg-name inst))
      (let ((target 
             (get-or-create-register 
              machine 
              (assign-reg-name inst)))
            (value-exp (assign-value-exp inst)))
        (let ((value-proc
               (if (operation-exp? value-exp)
                   (make-operation-exp
                    value-exp 
                    machine
                    labels
                    operations)
                   (make-primitive-exp
                    (car value-exp)
                    machine
                    labels))))
          (lambda ()   ; execution procedure
            ; for assign
            (set-contents! target (value-proc))
            (advance-pc pc))))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define 
  (make-test 
   inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition 
                machine
                labels
                operations)))
          (lambda () 
            (set-contents! 
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: 
                ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define 
  (make-branch 
   inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label 
                labels 
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: 
                ASSEMBLE"
               inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label 
                   labels
                   (label-exp-label dest))))
             (lambda () 
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-or-create-register 
                   machine
                   (register-exp-reg dest))))
             (lambda ()
               (set-contents! 
                pc
                (get-contents reg)))))
          (else (error "Bad GOTO instruction: 
                        ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-or-create-register 
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-or-create-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name 
         stack-instruction)
  (cadr stack-instruction))

(define (make-perform 
         inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: 
                ASSEMBLE"
               inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label 
                 labels
                 (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-or-create-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: 
                      ASSEMBLE"
                     exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp) 
  (cadr exp))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim 
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp 
                 e machine labels))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))


