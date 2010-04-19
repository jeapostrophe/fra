#lang scheme

(define-struct an-ivector ()
  #:transparent
  #:property prop:equal+hash
  (list (lambda (iv1 iv2 rec-equal?)
          (and (an-ivector? iv2)
               (for/and ([i (in-range 0 (ivector-length iv1))])
                 (rec-equal? (ivector-ref iv1 i)
                             (ivector-ref iv2 i)))))
        (lambda (uiv1 rec-hash-code)
          (rec-hash-code uiv1))
        (lambda (uiv1 rec-hash2-code)
          (rec-hash2-code uiv1))))
(define-struct (vec an-ivector) (vec)
  #:transparent)
(define-struct (union an-ivector) (iv1 iv1-len iv2)
  #:transparent)

(define ivector? an-ivector?)
(define (ivector . elems)
  (make-vec (apply vector-immutable elems)))
(define (ivector-ref iv n)
  (match iv
    [(struct vec (v))
     (vector-ref v n)]
    [(struct union (iv1 iv1-len iv2))
     (if (n . < . iv1-len)
         (ivector-ref iv1 n)
         (ivector-ref iv2 (- n iv1-len)))]))
(define (ivector-union iv1 iv2)
  (make-union iv1 (ivector-length iv1) iv2))
(define (ivector-length iv)
  (match iv
    [(struct vec (v))
     (vector-length v)]
    [(struct union (_ iv1-len iv2))
     (+ iv1-len (ivector-length iv2))]))

(provide/contract
 [ivector? (any/c . -> . boolean?)]
 [ivector (() () #:rest (listof any/c) . ->* . ivector?)]
 [ivector-length (ivector? . -> . exact-nonnegative-integer?)]
 ; XXX stronger contract
 [ivector-ref (ivector? exact-nonnegative-integer? . -> . any/c)]
 [ivector-union (ivector? ivector? . -> . ivector?)])
