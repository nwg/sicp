#lang racket

(require "../../utility.rkt")

(provide (all-defined-out))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((eq? exp '*unassigned*) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (make-assignment var value)
  (list 'set! var value))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))

(define (application? exp) (pair? exp))
(define (application-simple? exp) (and (pair? exp) (symbol? (car exp))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (last-operand? ops) (null? (cdr ops)))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-definitions exp)
  (cadr exp))

(define (let-actions exp)
  (cddr exp))

(define (make-let vars values actions)
  (let ([definitions (map list vars values)])
    (cons 'let (cons definitions actions))))

(define (let->combination exp)
  (let* ([definitions (let-definitions exp)]
         [vars (map car definitions)]
         [initials (map cadr definitions)]
         [body (let-actions exp)])
    (cons (make-lambda vars body) initials)))

(define (let*? exp)
  (tagged-list? exp 'let*))
(define (let*-definitions exp)
  (cadr exp))
(define (let*-actions exp)
  (cddr exp))

(define (let*->nested-lets exp)
  (define (helper definitions)
    (let ([var (caar definitions)]
          [val (cadar definitions)])
      (if (null? (cdr definitions))
          (let* ([body (let*-actions exp)]
                 [lamb (make-lambda (list var) body)])
            (list lamb val))
          (let ([lamb (make-lambda (list var) (list (helper (cdr definitions))))])
            (list lamb val)))))
  (let ([definitions (let*-definitions exp)])
    (if (null? definitions)
        (make-let '() (let*-actions exp))
        (helper definitions))))

(define (scan-out-defines body)
  (let-values ([(defines rest) (splitf-at body definition?)])
    (if (null? defines)
        rest
        (if (findf definition? rest)
            (error "Definitions must come first in body")
            (let* ([vars (map definition-variable defines)]
                   [values (map definition-value defines)]
                   [let-vals (make-list (length vars) '*unassigned*)]
                   [sets (map (lambda (var value) (make-assignment var value)) vars values)])
              (list
               (make-let
                vars
                let-vals
                (append
                 sets
                 rest))))))))

(define (apply? exp)
  (tagged-list? exp 'apply))
(define (apply-proc exp)
  (cadr exp))
(define (apply-args exp)
  (caddr exp))

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-conditions exp) (cdr exp))

(define (and->if exp)
  (define (helper conditions)
    (if (null? conditions)
        'true
        (make-if (car conditions) (helper (cdr conditions)) 'false)))
  (helper (and-conditions exp)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-conditions exp) (cdr exp))

(define (or->if exp)
  (define (helper conditions)
    (if (null? conditions)
        'false
        (make-if (car conditions) 'true (helper (cdr conditions)))))
  (helper (or-conditions exp)))
