#lang racket

(define ex5.2
  '(controller
      (assign counter (const 1))
      (assign product (const 1))
    test-counter      
      (test (op <) (reg n) (reg counter))
      (branch (label fact-done))
      (assign t (op *) (reg product) (reg counter))
      (assign counter (op +) (reg counter) (const 1))
      (assign product (reg t))
      (goto (label test-counter))

    fact-done))