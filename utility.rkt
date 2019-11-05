#lang racket

(require racket/set)

(provide rm-dupes)
(provide groupby)

(define rm-dupes (compose set->list list->set))

(define (groupby keyfunc items)
  (define (reduce item grouped)
    (let-values ([(front back)
                  (splitf-at
                   grouped
                   (lambda (g)
                     (not (equal? (car g) (keyfunc item)))))])
      (if (null? back)
          (cons (list (keyfunc item) (list item)) front)
          (let* ([existing-insts (cadar back)]
                 [key (caar back)])
            (append
             front
             (list (list key (cons item existing-insts)))
             (cdr back))))))
  (foldr reduce '() items))
            