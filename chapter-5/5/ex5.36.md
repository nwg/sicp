The order is right-to-left. This is determined by `construct-arglist`.

The implementation given in the code uses cons to make a reverse list and then
a reverse builtin The reverse operation is order n and each arg cons is order 1
so the time taken is roughly 2n instead of n, for the original implementation.
