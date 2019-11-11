#lang racket

(require "../2/base.rkt")


(define count-leaves-machine
  (make-machine
   (list (list 'pair? pair?) (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'not not) (list '+ +))
   '(  (assign continue (label done))
     count-leaves
       (test (op null?) (reg tree))
       (branch (label is-null))
       (goto (label not-null))
     is-null
       (assign val (const 0))
       (goto (reg continue))
     not-null
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp))
       (branch (label not-pair))
       (goto (label is-pair))
     not-pair
       (assign val (const 1))
       (goto (reg continue))
     is-pair
       (save continue)
       (save tree)
       (assign tree (op car) (reg tree))
       (assign continue (label after-count-leaves-1))
       (goto (label count-leaves))
     after-count-leaves-1
       (restore tree)
       (save val)
       (assign tree (op cdr) (reg tree))
       (assign continue (label after-count-leaves-2))
       (goto (label count-leaves))
     after-count-leaves-2
       (assign tmp (reg val))
       (restore val)
       (assign val (op +) (reg tmp) (reg val))
       (restore continue)
       (goto (reg continue))

     done)))

#|
(define test-tree (cons (cons 1 2) (cons (cons 3 '()) (cons 4 5))))
;(define test-tree 2)
(set-register-contents! count-leaves-machine 'tree test-tree)
;(count-leaves-machine 'trace-on)
(start count-leaves-machine)
(display "count-leaves-machine: ")
(display (get-register-contents count-leaves-machine 'val))
(newline)
|#

(define count-leaves-iter-machine
  (make-machine
   (list (list 'pair? pair?) (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'not not) (list '+ +))
   '(  (assign continue (label done))
       (assign n (const 0))
     count-leaves
       (test (op null?) (reg tree))
       (branch (label is-null))
       (goto (label not-null))
     is-null
       (assign val (reg n))
       (goto (reg continue))
     not-null
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp))
       (branch (label not-pair))
       (goto (label is-pair))
     not-pair
       (assign val (op +) (reg n) (const 1))
       (goto (reg continue))
     is-pair
       (save tree)
       (save continue)
       (assign tree (op car) (reg tree))
       (assign continue (label after-count-leaves))
       (goto (label count-leaves))
     after-count-leaves
       (restore continue)
       (restore tree)
       (assign tree (op cdr) (reg tree))
       (assign n (reg val))
       (goto (label count-leaves))

     done)))

#|
;(define test-tree (cons (cons 1 2) (cons (cons 3 '()) (cons 4 5))))
(set-register-contents! count-leaves-iter-machine 'tree test-tree)
(count-leaves-iter-machine 'trace-on)
((count-leaves-iter-machine 'trace-register) 'n)
((count-leaves-iter-machine 'trace-register) 'tree)
((count-leaves-iter-machine 'trace-register) 'continue)
(start count-leaves-iter-machine)
(display "count-leaves-iter-machine: ")
(display (get-register-contents count-leaves-iter-machine 'val))
(newline)
|#

(define append-machine
  (make-machine
   (list (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'cons cons))
   '(  (assign continue (label done))
     append
       (test (op null?) (reg x))
       (branch (label is-null))
       (goto (label not-null))
     is-null
       (assign val (reg y))
       (goto (reg continue))
     not-null
       (save x)
       (save continue)
       (assign x (op cdr) (reg x))
       (assign continue (label after-append))
       (goto (label append))
     after-append
       (restore continue)
       (restore x)
       (assign tmp (op car) (reg x))
       (assign val (op cons) (reg tmp) (reg val))
       (goto (reg continue))
     done)))

(set-register-contents! append-machine 'x '(1 2 3))
(set-register-contents! append-machine 'y '(4 5 6))
(start append-machine)
(display "append-machine: ")
(display (get-register-contents append-machine 'val))
(newline)

(define append!-machine
  (make-machine
   (list (list 'null? null?) (list 'cdr mcdr) (list 'set-cdr! set-mcdr!))
   '(  (assign continue (label done))
     append!
       (save x)
       (save continue)
       (assign continue (label last-pair-done))
       (goto (label last-pair))
     last-pair-done
       (perform (op set-cdr!) (reg val) (reg y))
       (restore continue)
       (restore val)
       (goto (reg continue))

     last-pair
       (assign tmp (op cdr) (reg x))
       (test (op null?) (reg tmp))
       (branch (label is-null))
       (goto (label not-null))
     is-null
       (assign val (reg x))
       (goto (reg continue))
     not-null
       (assign x (op cdr) (reg x))
       (goto (label last-pair))

     done)))

(require compatibility/mlist)


(set-register-contents! append!-machine 'x (mlist 1 2 3))
(set-register-contents! append!-machine 'y (mlist 4 5 6))
(start append!-machine)
(display "append!-machine: ")
(display (get-register-contents append!-machine 'val))
(newline)


;(define test (mlist 1 2 3))
;(mcdr test)
