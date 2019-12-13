|                           | Maximum Depth |    Number of Pushes   |
|---------------------------|:-------------:|:---------------------:|
| Interpreted               |   5(n-1) + 8  |      16(n-1) + 32     |
| Special-purpose           |     2n - 2    |         2n - 2        |
| Compiled (w/ open coding) |       3n      | 2(n-2) + 3 for n >= 2 |

1. * Compiled to interpreted:
     * The max-depth ratio 3/5
     * The pushes ratio is 2/16
   * For Special-purpose to interpreted:
     * max-depth is 2/5
     * pushes is 2/16

   Note that pushes for compiled/interpreted and specal-purpose/interpreted are both 2/16, so the open-coding compiler does
   as well as the special-purpose machine for this case.
2. For the number of pushes, the open-coding compiler already does as well as the special-purpose machine
   For the max-depth, an extra save/restore exists while looking up the 'factorial' name in the global environment.
   If we didn't allow redefinition of names then the compiler could scan all names and their
   entry points, not allow redefinition of names and simply directly assign the entry point to val without an env lookup.
   Then, in the case of the open-coding compiler, env would not need to be preserved around the global env lookup.

   So, in the code:

   ```lisp
   (save env)
   (assign env (op get-global-environment))
   (assign proc (op lookup-variable-value) (const factorial) (reg env))
   (restore env)
   ```

   would become
   ```lisp
   (assign proc (label <compiled-entry-point-of-factorial>))
   ```

