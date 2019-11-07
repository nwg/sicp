# hand-simulate factorial 3

n <- 3
continue <- 'fact-done

stack:
 'fact-done
 3

n <- 2
continue <- 'after-fact

stack:
 'fact-done
 3
 'after-fact
 2

n <- 1
continue <- 'after-fact

val <- 1
goto 'after-fact
n <- 2
continue <- 'after-fact
stack:
 'fact-done
 3

val <- 2 * 1
goto 'after-fact
n <- 3
continue <- 'fact-done
stack:
 <empty>

val <- 3 * 2 = 6
goto 'fact-done


# hand-simulate fib 4

n <- 4
continue <- 'fib-done

stack:
 'fib-done
 4

continue <- 'afterfib-n-1
n <- n - 1 = 3
goto fib-loop

stack:
 'fib-done
 4
 'afterfib-n-1
 3

continue <- 'afterfib-n-1
n <- n - 1 = 2
goto fib-loop

push continue
push n

stack:
 'fib-done
 4
 'afterfib-n-1
 3
 'afterfib-n-1
 2

continue <- 'afterfib-n-1
n <- n - 1 = 1
goto fib-loop

val <- 1
goto continue = 'afterfib-n-1

n <- pop 2
continue <- pop 'afterfib-n-1

stack:
 'fib-done
 4
 'afterfib-n-1
 3

n <- n - 2 = 0

push continue
push val

stack:
 'fib-done
 4
 'afterfib-n-1
 3
 'afterfib-n-1
 1

continue <- 'afterfib-n-2
goto fib-loop

val <- 0
goto continue = 'afterfib-n-2

n <- val = 0
val <- pop = 1
continue <- pop = 'afterfib-n-1

stack:
 'fib-done
 4
 'afterfib-n-1
 3

val <- val + n = 1 + 0 = 1
goto continue = 'afterfib-n-1

n <- pop = 3
continue <- pop = 'afterfib-n-1

stack:
 'fib-done
 4

n <- n - 2 = 1
push continue = 'afterfib-n-1
push val = 1

stack:
 'fib-done
 4
 'afterfib-n-1
 1

continue <- afterfib-n-2
goto 'fib-loop

val <- n = 1
goto continue = 'afterfib-n-2

n <- val = 1
val <- pop = 1
continue <- pop = 'afterfib-n-1

stack:
 'fib-done
 4

val <- val + n = 1 + 1 = 2
goto continue = 'afterfib-n-1

n <- pop = 4
continue <- pop = 'fib-done

stack:
 <empty>

n <- n - 2 = 2
push continue = 'fib-done
push val = 2

stack:
 'fib-done
 2

continue <- 'afterfib-n-2
goto 'fib-loop

push continue = 'afterfib-n-2
push n = 2

stack:
 'fib-done
 2
 'afterfib-n-2
 2

continue <- 'afterfib-n-1
n <- n - 1 = 1
goto 'fib-loop

val <- n = 1
goto continue = 'afterfib-n-1

n <- pop = 2
continue <- pop = 'afterfib-n-2

stack:
 'fib-done
 2

n <- n - 2 = 0
push continue = 'afterfib-n-2
push val = 1

stack:
 'fib-done
 2
 'afterfib-n-2
 1

goto 'fib-loop

val <- n = 0
goto continue = 'afterfib-n-2

n <- val = 0
val <- pop = 1
continue <- pop = 'afterfib-n-2

stack:
 'fib-done
 2

val <- val + n = 1 + 0 = 1
goto continue = 'afterfib-n-2

n <- val = 1
val <- pop = 2
continue <- pop = 'fib-done
val <- val + n = 2 + 1 = 3
goto continue = 'fib-done

-- fib 4 = 3

