1. see [code](https://github.com/nwg/sicp/blob/ex5.38/chapter-5/5/compiler.rkt) and [diff](https://github.com/nwg/sicp/commit/fcedb25d917c4bff1da9c0630653af4ed35fb016#diff-1520bc4ef2b0995ca0ae523d35f5626d)
2. see [code](https://github.com/nwg/sicp/blob/ex5.38/chapter-5/5/compiler.rkt) and [diff](https://github.com/nwg/sicp/commit/fcedb25d917c4bff1da9c0630653af4ed35fb016#diff-1520bc4ef2b0995ca0ae523d35f5626d)
3. The old compiler emits a normal procedure call with the two (compiled and primtive) branches
   for ops =, *, and -. There are more stack saves. For example, proc is sometimes saved.
   Env and continue are saved. Sometimes proc is saved. argl is set up.
   In the new version, the two branches are not generated, env and continue and proc do
   not need to be saved. Args still need to be set up but are set up in a different way.
   The intermediate val assignment is not needed so there are half the arg setup instructions.
   ```lisp
   (assign val (op make-compiled-procedure) (label entry1) (reg env))                (assign val (op make-compiled-procedure) (label entry1) (reg env))
   (goto (label after-lambda2))                                                      (goto (label after-lambda2))
   entry1                                                                            entry1
   (assign env (op compiled-procedure-env) (reg proc))                               (assign env (op compiled-procedure-env) (reg proc))
   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))             (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
   (assign arg1 (op lookup-variable-value) (const n) (reg env))                |     (save continue)
   (assign arg2 (const 1))                                                     |     (save env)
   (assign val (op =) (reg arg1) (reg arg2))                                   |     (assign proc (op lookup-variable-value) (const =) (reg env))
                                                                               >     (assign val (const 1))
                                                                               >     (assign argl (op list) (reg val))
                                                                               >     (assign val (op lookup-variable-value) (const n) (reg env))
                                                                               >     (assign argl (op cons) (reg val) (reg argl))
                                                                               >     (test (op primitive-procedure?) (reg proc))
                                                                               >     (branch (label primitive-branch6))
                                                                               >     compiled-branch7
                                                                               >     (assign continue (label after-call8))
                                                                               >     (assign val (op compiled-procedure-entry) (reg proc))
                                                                               >     (goto (reg val))
                                                                               >     primitive-branch6
                                                                               >     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
                                                                               >     after-call8
                                                                               >     (restore env)
                                                                               >     (restore continue)
   (test (op false?) (reg val))                                                      (test (op false?) (reg val))
   (branch (label false-branch4))                                                    (branch (label false-branch4))
   true-branch3                                                                      true-branch3
   (assign val (const 1))                                                            (assign val (const 1))
   (goto (reg continue))                                                             (goto (reg continue))
   false-branch4                                                                     false-branch4
                                                                               >     (assign proc (op lookup-variable-value) (const *) (reg env))
   (save continue)                                                                   (save continue)
   (assign arg1 (op lookup-variable-value) (const n) (reg env))                |     (save proc)
   (save arg1)                                                                 |     (save env)
   (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))          (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
   (assign arg1 (op lookup-variable-value) (const n) (reg env))                |     (save proc)
   (assign arg2 (const 1))                                                     |     (assign proc (op lookup-variable-value) (const -) (reg env))
   (assign val (op -) (reg arg1) (reg arg2))                                   |     (assign val (const 1))
   (assign argl (op list) (reg val))                                                 (assign argl (op list) (reg val))
                                                                               >     (assign val (op lookup-variable-value) (const n) (reg env))
                                                                               >     (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))                                       (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch6))                                          |     (branch (label primitive-branch9))
   compiled-branch7                                                            |     compiled-branch10
   (assign continue (label proc-return9))                                      |     (assign continue (label after-call11))
   (assign val (op compiled-procedure-entry) (reg proc))                             (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))                                                                  (goto (reg val))
   proc-return9                                                                |     primitive-branch9
   (assign arg2 (reg val))                                                     |     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   (goto (label after-call8))                                                  |     after-call11
   primitive-branch6                                                           |     (assign argl (op list) (reg val))
   (assign arg2 (op apply-primitive-procedure) (reg proc) (reg argl))          |     (restore proc)
   after-call8                                                                 |     (test (op primitive-procedure?) (reg proc))
   (restore arg1)                                                              |     (branch (label primitive-branch12))
   (assign val (op *) (reg arg1) (reg arg2))                                   |     compiled-branch13
                                                                               >     (assign continue (label after-call14))
                                                                               >     (assign val (op compiled-procedure-entry) (reg proc))
                                                                               >     (goto (reg val))
                                                                               >     primitive-branch12
                                                                               >     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
                                                                               >     after-call14
                                                                               >     (assign argl (op list) (reg val))
                                                                               >     (restore env)
                                                                               >     (assign val (op lookup-variable-value) (const n) (reg env))
                                                                               >     (assign argl (op cons) (reg val) (reg argl))
                                                                               >     (restore proc)
   (restore continue)                                                                (restore continue)
                                                                               >     (test (op primitive-procedure?) (reg proc))
                                                                               >     (branch (label primitive-branch15))
                                                                               >     compiled-branch16
                                                                               >     (assign val (op compiled-procedure-entry) (reg proc))
                                                                               >     (goto (reg val))
                                                                               >     primitive-branch15
                                                                               >     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
   (goto (reg continue))                                                             (goto (reg continue))
                                                                               >     after-call17
   after-if5                                                                         after-if5
   after-lambda2                                                                     after-lambda2
   (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))         (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
   (assign val (const ok))                                                           (assign val (const ok))
   ```
4. see [code](https://github.com/nwg/sicp/blob/ex5.38/chapter-5/5/compiler.rkt) and [diff](https://github.com/nwg/sicp/commit/c8ded4aa4d06fa695bfed2a240094bc4a0c0a93f#diff-1520bc4ef2b0995ca0ae523d35f5626d)