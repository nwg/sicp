2.
  - Although you could build many optimizations, you can never fully match compilation because
     an interpreter still has to discover what type of expression is being evaluated.
     This means for the expression `(f 84 96)` we still have to go through the eval-dispatch chain
     to figure out it's an application. I can't think of a way to avoid eval-dispatch in the interpreter.
     Also, we still have to test for end of list when iterating for each operand.
     You couldn't really special case a function of, say, exactly two args because this would amount to examining
     the length of the args.
  -  There are certain things you could do:
     A compiler can exploit the structure of the particular expression it is processing to
     generate code that avoids unnecessary stack operations.
     However, you could build similar discovery code into an interpreter. This would
     amount to basically compiling the code of and caching the register usage of the body of each lambda.
     Then you could avoid unneeded stack operations by referencing
     the cached modifies list before each application.
