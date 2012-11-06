#lang racket

(define-syntax (matc stx)
  (syntax-case stx ()
    [(_ x [() body] ...) #'(match x [(list) body] ...)]
    [(_ x [(pa ...) body] ...)
     #'(match x [(quasiquote ((unquote pa) ...)) body] ...)]
    [(_ x [(pa ... . rest) body] ...)
     #'(match x [(quasiquote ((unquote pa) ... . (unquote rest))) body] ...)]))

(define-for-syntax (clause-rewrite cl)
  (syntax-case cl ()
    [[() body ...] #'[(list) body ...]]
    [[(x ...) body ...] #'[(quasiquote ((unquote x) ...)) body ...]]
    [[(x ... . rest) body ...] #'[(quasiquote ((unquote x) ... . (unquote rest))) body ...]]))
                                
(provide new-match)
(define-syntax (new-match stx)
  (syntax-case stx ()
    [(_ val-expr clause ...) 
     (let-values
      ([(stx stx-opaq)
        (with-handlers 
            ([exn:fail:syntax? 
              (λ (e) 
                (with-syntax
                    ([(new-cl ...) (map clause-rewrite (syntax->list #'(clause ...)))])
                  (syntax-local-expand-expression 
                   #'(match val-expr new-cl ...))))])
          (syntax-local-expand-expression 
           #'(match val-expr clause ...)))])
      stx)]))

(matc (list 1 2 3 4)
        [(x y a z) y])

(matc (list 1 2 3 4)
        [(x y . rest) y])

(matc (list 1 2 3 4)
      [(x y . rest) rest])

(define (length lst)
  (new-match lst
    [() 0]
    [(x . xs) (add1 (length xs))]))

(define (length2 lst)
  (match lst
    [(list) 0]
    [(list x xs ...) (add1 (length2 xs))]))
(define (length3 lst)
  (match lst
    ['() 0]
    [(list-rest x xs) (add1 (length3 xs))]))

(define (filter p? lst)
  (new-match lst
    [() '()]
    [((? p? x) . xs) (cons x (filter p? xs))]
    [(_ . xs) (filter p? xs)]))

(define (filter2 p? lst)
  (match lst
    [(list) null]
    [(list (? p? x) xs ...) (cons x (filter2 p? xs))]
    [(list _ xs ...) (filter2 p? xs)]))

(require rackunit)
(define lst1 '(1))
(define lst2 '(1 2 3 4 5))
(check-true (= (length null) (length2 null) (length3 null) 0))
(check-true (= (length lst1) (length2 lst1) (length3 lst1) 1))
(check-true (= (length lst2) (length2 lst2) (length3 lst2) 5))