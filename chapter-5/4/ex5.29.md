1. 5(n-2) + 13
1.  1. - S(0) = 16
       - S(1) = 16
       - S(n) = S(n-2) + S(n-1) + 40
    1. S(n) = 56*fib(n+1) - 40

Proof of part 2.ii
```
S(0) = 16
S(1) = 16
S(2) = 72
S(3) = 128

assume S(n-2) is of form a*fib(n-2+1) and S(n-1) is of form a*fib(n-1+1)
then S(n-2) = a*fib(n-1) + b and S(n-1) = a*fib(n) + b
S(n) = S(n-2) + S(n-1) + 40 = a*fib(n-1) + a*fib(n) + 2b + 40
= a*fib(n+1) + 2b + 40
if b=-40 then S(n) is also of form a*fib(n+1)
because S(n) = a*fib(n+1) - 80 + 40 = a*fib(n+1) - 40
then subbing for example S(2) we have S(2) = a*fib(3) - 40 = 72 and a=56
since S(0) and S(1) are of this form, then
for all n:
S(n) = 56*fib(n+1) - 40
```
