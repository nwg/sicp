#lang racket

(require racket/set)

(provide rm-dupes)
(provide groupby)
(provide mcadr)
(provide mcddr)
(provide mcaddr)
(provide mcdddr)

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
            
(define mcadr (compose mcar mcdr))
(define mcddr (compose mcdr mcdr))
(define mcaddr (compose mcar mcdr mcdr))
(define mcdddr (compose mcdr mcddr))
