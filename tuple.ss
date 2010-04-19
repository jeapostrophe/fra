#lang scheme
(require "ivector.ss")

(define-struct tuple (ivec) #:prefab)

(define tuple-disjoint-union
  (match-lambda*
    [(list (struct tuple (ivec1))
           (struct tuple (ivec2)))
     (make-tuple (ivector-union ivec1 ivec2))]))

(define (tuple-ref tup n)
  (ivector-ref (tuple-ivec tup) n))

(define (tuple-length tup)
  (ivector-length (tuple-ivec tup)))

(define (tuple-proj indices tup)
  (make-tuple
   (apply ivector
          (for/list ([i (in-list indices)])
            (tuple-ref tup i)))))

(define (singleton-tuple how-long elem)
  (make-tuple (apply ivector (make-list how-long elem))))

(define-syntax Tuple
  (syntax-rules ()
    [(_ value ...)
     (create-tuple value ...)]))

(define (create-tuple . v)
  (make-tuple (apply ivector v)))

(provide/contract
 [tuple? (any/c . -> . boolean?)]
 [rename create-tuple tuple (() () #:rest (listof any/c) . ->* . tuple?)]
 [tuple-disjoint-union (tuple? tuple? . -> . tuple?)]
 [tuple-length (tuple? . -> . exact-nonnegative-integer?)]
 [tuple-ref (tuple? exact-nonnegative-integer? . -> . any/c)]
 [tuple-proj ((listof exact-nonnegative-integer?) tuple? . -> . tuple?)]
 [singleton-tuple (exact-nonnegative-integer? any/c . -> . tuple?)])
(provide
 Tuple)