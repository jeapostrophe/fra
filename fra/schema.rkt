#lang racket/base
(require racket/contract
         racket/list
         racket/set)

;; XXX These seem pretty inefficient

(define (schema . elems)
  (apply list elems))
(define (schema-length s)
  (length s))
(define schema/c
  (listof symbol?))
(define (schema-ref s a)
  (index a s))

(define (schema-disjoint-union s1 s2)
  (append s1 s2))

(define (schema-replace* renaming s)
  (for/list ([old (in-list s)])
    (hash-ref renaming old old)))

(define (hash-keys ht) (hash-map ht (lambda (k v) k)))
(define (hash-values ht) (hash-map ht (lambda (k v) v)))
(define (no-duplicates? s)
  (define seen? (make-hasheq))
  (for/and ([e (in-list s)])
    (begin0 (hash-ref seen? e #t)
            (hash-set! seen? e #f))))

(define (schema-valid-renaming? renaming s)
  (and (schema-subset? (hash-keys renaming) s)
       (no-duplicates? (hash-values renaming))))

(define (index a s)
  (let loop ([n 0]
             [s s])
    (if (empty? s)
      (error 'index "Not an element: ~e" a)
      (if (equal? (first s) a)
        n
        (loop (add1 n) (rest s))))))

(define (schema-intersection s1 s2)
  (set->list
   (set-intersect (list->set s1)
                  (list->set s2))))
(define (schema-difference s1 s2)
  (set->list
   (set-subtract (list->set s1)
                 (list->set s2))))
(define (schema-union s1 s2)
  (set->list
   (set-union (list->set s1)
              (list->set s2))))
(define (schema-subset? s1 s2)
  (subset? (list->set s1)
           (list->set s2)))

(define (schema-disjoint? s1 s2)
  (set-empty? (set-intersect (list->set s1) (list->set s2))))
(define (schema-orderi-equal? s1 s2)
  (set=? (list->set s1) (list->set s2)))

(provide/contract
 [schema/c contract?]
 [schema (() () #:rest (listof symbol?) . ->* . schema/c)]
 [schema-length (schema/c . -> . exact-nonnegative-integer?)]
 [schema-ref (schema/c symbol? . -> . exact-nonnegative-integer?)]
 [schema-replace* ((hash/c symbol? symbol?) schema/c . -> . schema/c)]
 [schema-valid-renaming? ((hash/c symbol? symbol?) schema/c . -> . boolean?)]
 [schema-disjoint? (schema/c schema/c . -> . boolean?)]
 [schema-orderi-equal? (schema/c schema/c . -> . boolean?)]
 [schema-subset? (schema/c schema/c . -> . boolean?)]
 [schema-disjoint-union (schema/c schema/c . -> . schema/c)]
 [schema-union (schema/c schema/c . -> . schema/c)]
 [schema-intersection (schema/c schema/c . -> . schema/c)]
 [schema-difference (schema/c schema/c . -> . schema/c)])
