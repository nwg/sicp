The order is right-to-left. This is determined by `construct-arglist`.

The implementation given in the [code](https://github.com/nwg/sicp/commit/fca6f84f5a32b35fb068dc2dfb02b9208c4e00c6#diff-1520bc4ef2b0995ca0ae523d35f5626d) uses cons to make a reverse list and then
a reverse builtin The reverse operation is order n and each arg cons is order 1
so the time taken is roughly 2n instead of n, for the original implementation.
