#lang racket

(require racket/set)

(provide rm-dupes)
(provide groupby)
(provide mcadr)
(provide mcddr)
(provide mcaddr)
(provide mcdddr)
(provide tagged-list?)
(provide data)
(provide tag)
(provide truncate)
(provide truncate-for-display)

(define (truncate ls len)
  ;;; returns (values truncated-ls was-short did-truncate)
  (define was-short false)
  (let-values ([(truncated back)
                (with-handlers
                  ([exn:fail:contract?
                    (λ (e)
                      (set! was-short true)
                      (values ls '()))])
                  (split-at ls len))])
    (values truncated was-short back)))

(define (truncate-for-display seq sep len [to-string ~s])
  (let-values ([(truncated was-short back) (truncate seq len)])
    (let* ([did-truncate (not (null? back))]
           [display-tail
            (if did-truncate
                " ..."
                "")])
      (string-append (string-join (map to-string truncated) sep) display-tail))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (tag x) (car x))
(define (data x) (cdr x))

(define rm-dupes (compose set->list list->set))

(define (get-type x)
  (cond ((number? x) 'number)
        ((pair? x) 'pair)
        ((string? x) 'string)
        ((list? x) 'list)
        ((mpair? x) 'mpair)
        ((null? x) 'null)
        ((symbol? x) 'symbol)
        (else 'other)))

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
