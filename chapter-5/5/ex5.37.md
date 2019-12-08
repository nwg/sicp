For the simple code `(define x 2)` (shown below), we have many extra saves and
restores.  Although none of the code has linkage return, continue is saved
because end-with-linkage preserves continue in case linkage 'return' is used.
Since end-with-linkage is used by `compile-self-evaluating` and
`compile-definition`, continue is preserved twice. Register env is preserved
across the self evaluating value (`(assign val (const 2))`) because env is
preserved by compile-definition when it appends the `get-value-code` to the 
`define-variable!` code.




```lisp
  ---- Original `preserving` ----                                    ---- Always-preserve preserving ----
  ((assign val (const 2))                                       |    ((save continue)
                                                                >     (save env)
                                                                >     (save continue)
                                                                >     (assign val (const 2))
                                                                >     (restore continue)
                                                                >     (restore env)
   (perform (op define-variable!) (const x) (reg val) (reg env)       (perform (op define-variable!) (const x) (reg val) (reg env)
   (assign val (const ok)))                                     |     (assign val (const ok))
                                                                >     (restore continue))
```
