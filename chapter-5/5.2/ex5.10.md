We modify op to be a single expression

e.g. `(test (op = (reg n) (const 0)))` instead of `(test (op =) (reg n) (const 0))`

We also modify assignment to have source before destination

e.g. `(assign (label after-expt) continue)`
instead of `(assign (label after-expt) continue)`
