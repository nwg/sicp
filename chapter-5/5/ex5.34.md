```lisp
  ; *** recursive factorial
  ((assign val (op make-compiled-procedure) (label entry1) (reg env))
   (goto (label after-lambda2))
   entry1 ; *** this is the entry point for recursive factorial
   (assign env (op compiled-procedure-env) (reg proc))
   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
   (save continue)
   (save env)
   (assign proc (op lookup-variable-value) (const =) (reg env))
   (assign val (const 1))
   (assign argl (op list) (reg val))
   (assign val (op lookup-variable-value) (const n) (reg env))
   (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch6))
   compiled-branch7
   (assign continue (label after-call8))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch6
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call8
   (restore env)
   (restore continue)
   (test (op false?) (reg val))
   (branch (label false-branch4))
   true-branch3
   (assign val (const 1))
   (goto (reg continue)) ; *** this is the base case where we start popping stack values
   false-branch4
   (assign proc (op lookup-variable-value) (const *) (reg env))
   (save continue)
   (save proc) ; *** save proc *
   (save env)
   (assign proc (op lookup-variable-value) (const factorial) (reg env))
   (save proc) ; *** save proc recursive factorial
   (assign proc (op lookup-variable-value) (const -) (reg env))
   (assign val (const 1))
   (assign argl (op list) (reg val))
   (assign val (op lookup-variable-value) (const n) (reg env))
   (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch9))
   compiled-branch10
   (assign continue (label after-call11))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch9
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call11
   (assign argl (op list) (reg val))
   (restore proc) ; *** restore proc recursive factorial
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch12))
   compiled-branch13
   (assign continue (label after-call14)) ; *** inner factorial will continue at after-call14
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val)) ; *** call recursive factorial -- note excess env from entry, proc=*, continue from entry on stack
   primitive-branch12
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call14
   (assign argl (op list) (reg val)) ; *** get recursive factorial value
                                     ; *** this is first jumped to after true case in call to base case factorial n=1
   (restore env) ; *** restore env from entry
   (assign val (op lookup-variable-value) (const n) (reg env))
   (assign argl (op cons) (reg val) (reg argl))
   (restore proc) ; *** restore proc *
   (restore continue) ; *** restore original continue from entry
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch15))
   compiled-branch16
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch15
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   (goto (reg continue)) ; *** goto continue from entry with val=(* n (factorial (- n 1)))
                         ; *** env, proc, and continue have been popped from stack
   after-call17
   after-if5
   after-lambda2
   (perform (op define-variable!) (const factorial) (reg val) (reg env))
   (assign val (const ok))))


  ; *** iterative factorial
  ((assign val (op make-compiled-procedure) (label entry18) (reg env))
   (goto (label after-lambda19))
   entry18 ; *** this is the entry point jumped to by the tail-recursive call below
   (assign env (op compiled-procedure-env) (reg proc))
   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
   (assign val (op make-compiled-procedure) (label entry20) (reg env))
   (goto (label after-lambda21))
   entry20
   (assign env (op compiled-procedure-env) (reg proc))
   (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
   (save continue)
   (save env)
   (assign proc (op lookup-variable-value) (const >) (reg env))
   (assign val (op lookup-variable-value) (const n) (reg env))
   (assign argl (op list) (reg val))
   (assign val (op lookup-variable-value) (const counter) (reg env))
   (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch25))
   compiled-branch26
   (assign continue (label after-call27))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch25
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call27
   (restore env)
   (restore continue)
   (test (op false?) (reg val))
   (branch (label false-branch23))
   true-branch22
   (assign val (op lookup-variable-value) (const product) (reg env))
   (goto (reg continue))
   false-branch23
   (assign proc (op lookup-variable-value) (const iter) (reg env))
   (save continue)
   (save proc)
   (save env)
   (assign proc (op lookup-variable-value) (const +) (reg env))
   (assign val (const 1))
   (assign argl (op list) (reg val))
   (assign val (op lookup-variable-value) (const counter) (reg env))
   (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch31))
   compiled-branch32
   (assign continue (label after-call33))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch31
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call33
   (assign argl (op list) (reg val))
   (restore env)
   (save argl)
   (assign proc (op lookup-variable-value) (const *) (reg env))
   (assign val (op lookup-variable-value) (const product) (reg env))
   (assign argl (op list) (reg val))
   (assign val (op lookup-variable-value) (const counter) (reg env))
   (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch28))
   compiled-branch29
   (assign continue (label after-call30))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch28
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   after-call30
   (restore argl)
   (assign argl (op cons) (reg val) (reg argl))
   (restore proc)
   (restore continue)
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch34))
   compiled-branch35
   (assign val (op compiled-procedure-entry) (reg proc))
                    ; *** this is where iter calls itself tail recursively
   (goto (reg val)) ; *** `continue` holds the proper return address and there are no excess items on the stack
   primitive-branch34
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   (goto (reg continue))
   after-call36
   after-if24
   after-lambda21
   (perform (op define-variable!) (const iter) (reg val) (reg env))
   (assign val (const ok))
   (assign proc (op lookup-variable-value) (const iter) (reg env))
   (assign val (const 1))
   (assign argl (op list) (reg val))
   (assign val (const 1))
   (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch37))
   compiled-branch38
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))
   primitive-branch37
   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   (goto (reg continue))
   after-call39
   after-lambda19
   (perform (op define-variable!) (const factorial) (reg val) (reg env))
   (assign val (const ok)))
```
