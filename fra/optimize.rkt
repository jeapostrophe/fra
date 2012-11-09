#lang racket/base
(require racket/match
         racket/contract
         "query.rkt"
         #;(except-in "query.rkt"
                    make-q:rename*
                    make-q:union
                    make-q:intersection
                    make-q:difference
                    make-q:product
                    make-q:projection
                    make-q:selection)
         "prop.rkt"
         "schema.rkt")

(define (hash-image ht)
  (for/hash ([(from to) (in-hash ht)])
    (values to from)))

(define (merge-renaming fst snd)
  (define fst-image (hash-image fst))
  (for/fold ([merged fst])
    ([(old new) (in-hash snd)])
    ; If it was created by the fst, then rename the original
    (if (hash-has-key? fst-image old)
        (hash-set merged (hash-ref fst-image old) new)
        (hash-set merged old new))))

; Debugging
#|
(define make-q:rename* query-rename*)
(define make-q:union query-union)
(define make-q:intersection query-intersection)
(define make-q:difference query-difference)
(define make-q:product query-product)
(define make-q:projection query-projection)
(define make-q:selection query-selection)
|#

(define pull-up/once
  (match-lambda
    ; Renaming
    ;; Distribution
    #| These rules actually only valid on a push-down (at least with how I've defined union-compatible)
    [(struct q:union ((struct q:rename* (old->new r))
                      (struct q:rename* (old->new s))))
     (make-q:rename* old->new (make-q:union r s))]
    [(struct q:difference ((struct q:rename* (old->new r))
                           (struct q:rename* (old->new s))))
     (make-q:rename* old->new (make-q:difference r s))]
    [(struct q:intersection ((struct q:rename* (old->new r))
                             (struct q:rename* (old->new s))))
     (make-q:rename* old->new (make-q:intersection r s))]
    ; Projection
    ;; Distribution
    [(struct q:union ((struct q:projection (schema r))
                      (struct q:projection (schema s))))
     (make-q:projection schema (make-q:union r s))]
    [(struct q:difference ((struct q:projection (schema r))
                           (struct q:projection (schema s))))
     (make-q:projection schema (make-q:difference r s))]
    [(struct q:intersection ((struct q:projection (schema r))
                             (struct q:projection (schema s))))
     (make-q:projection schema (make-q:intersection r s))]
    |#
    ;; Selection
    ;;; I am allowed to turn (project A (select P R)) into (select P (project A R)) if P only refers to As
    ;;;     The left is preferred when R is a cross product or join
    ;;;     The right is preferred otherwise    
    [(and (struct q:projection (schema (struct q:selection (prop r))))
          (? (lambda (q)
               (schema-subset? (prop-schema (q:selection-prop (q:projection-r q)))
                               (q:projection-schema q)))))               
     (make-q:selection prop (make-q:projection schema r))]
    ; Selection
    ;; Distribution
    [(struct q:difference ((struct q:selection (A R))
                           (struct q:selection (A P))))
     (make-q:selection A (make-q:difference R P))]
    [(struct q:difference ((struct q:selection (A R)) P))
     (make-q:selection A (make-q:difference R P))]
    [(struct q:union ((struct q:selection (A R))
                      (struct q:selection (A P))))
     (make-q:selection A (make-q:union R P))]
    [(struct q:intersection ((struct q:selection (A R))
                             (struct q:selection (A P))))
     (make-q:selection A (make-q:intersection R P))]
    [(struct q:intersection ((struct q:selection (A R)) P))
     (make-q:selection A (make-q:intersection R P))]
    [(struct q:intersection (R (struct q:selection (A P))))
     (make-q:selection A (make-q:intersection R P))]
    ; Complex Conditions
    [(struct q:union ((struct q:selection (A R))
                      (struct q:selection (B R))))
     (make-q:selection (make-prop:or A B) R)]
    [(struct q:selection (A (struct q:selection (B R))))
     (make-q:selection (make-prop:and A B) R)]
    ; Else
    [q
     q]))

(define simplify/once
  (match-lambda
    ; Renaming
    ;; Idempotentcy
    [(struct q:rename* (snd-old->new (struct q:rename* (fst-old->new inner-q))))
     (make-q:rename* (merge-renaming fst-old->new snd-old->new) inner-q)]
    ; Projection
    ;; Idempotentcy
    [(and (struct q:projection (snd-schema (struct q:projection (fst-schema q))))
          (? (lambda (q)
               (schema-subset? (q:projection-schema q)
                               (q:projection-schema (q:projection-r q))))))               
     ; side condition: snd-schema subset of fst-schema [always true for valid queries]
     (make-q:projection snd-schema q)]
    ; Selection
    ;; Idempotentcy
    [(struct q:selection (A (struct q:selection (A R))))
     (make-q:selection A R)]
    ;; Triviality
    [(struct q:selection ((? prop-trivial?) R))
     R]
    ; Else
    [q
     q]))

(define push-down/once
  (match-lambda
    ; Renaming
    ;; Distribution
    [(struct q:rename* (old->new (struct q:union (r s))))
     (make-q:union (make-q:rename* old->new r)
                   (make-q:rename* old->new s))]
    [(struct q:rename* (old->new (struct q:difference (r s))))
     (make-q:difference (make-q:rename* old->new r)
                        (make-q:rename* old->new s))]
    [(struct q:rename* (old->new (struct q:intersection (r s))))
     (make-q:intersection (make-q:rename* old->new r)
                          (make-q:rename* old->new s))]
    ; Projection
    ;; Distribution
    [(struct q:projection (schema (struct q:union (r s))))
     (make-q:union (make-q:projection schema r)
                   (make-q:projection schema s))]
    ; XXX I don't know why this doesn't work
    #;[(struct q:projection (schema (struct q:difference (r s))))
     (make-q:difference (make-q:projection schema r)
                        (make-q:projection schema s))]
    [(struct q:projection (schema (struct q:intersection (r s))))
     (make-q:intersection (make-q:projection schema r)
                          (make-q:projection schema s))]
    ;; Selection
    ;;; I am allowed to turn (project A (select P R)) into (select P (project A R)) if P only refers to As
    ;;;     The left is preferred when R is a cross product or join
    ;;;     The right is preferred otherwise    
    [(struct q:selection (prop (struct q:projection (schema r))))
     (make-q:projection schema (make-q:selection prop r))]
    ; Selection
    ;; Cross
    [(struct q:selection (A (struct q:product (R P))))
     (define-values (B C D) (prop-breakup A (query-schema R) (query-schema P)))
     (make-q:selection D (make-q:product (make-q:selection B R) (make-q:selection C P)))]     
    ;; Distribution
    [(struct q:selection (A (struct q:difference (R P))))
     (make-q:difference (make-q:selection A R)
                        ; This might make the selection available for a product
                        (make-q:selection A P))]
    [(struct q:selection (A (struct q:union (R P))))
     (make-q:union (make-q:selection A R)
                   (make-q:selection A P))]
    [(struct q:selection (A (struct q:intersection (R P))))
     (make-q:intersection (make-q:selection A R)
                          ; This might make the selection available for a product
                          (make-q:selection A P))]
    ; Else
    [q
     q]))

(define (bottom-up f q)
  (match q
    [(struct q:relation (id))
     q]
    [(struct q:singleton (schema))
     q]
    [(struct q:union (r s))
     (define r-after (bottom-up f r))
     (define s-after (bottom-up f s))
     (make-q:union (f r-after) (f s-after))]
    [(struct q:difference (r s))
     (define r-after (bottom-up f r))
     (define s-after (bottom-up f s))
     (make-q:difference (f r-after) (f s-after))]
    [(struct q:intersection (r s))
     (define r-after (bottom-up f r))
     (define s-after (bottom-up f s))
     (make-q:intersection (f r-after) (f s-after))]
    [(struct q:product (r s))
     (define r-after (bottom-up f r))
     (define s-after (bottom-up f s))
     (make-q:product (f r-after) (f s-after))]    
    [(struct q:projection (schema r))
     (define r-after (bottom-up f r))
     (make-q:projection schema (f r-after))]
    [(struct q:selection (prop r))
     (define r-after (bottom-up f r))
     (make-q:selection prop (f r-after))]
    [(struct q:rename* (old->new r))
     (define r-after (bottom-up f r))
     (make-q:rename* old->new (f r-after))]))

(define (top-down f q)
  (match (f q)
    [(struct q:relation (id))
     q]
    [(struct q:singleton (schema))
     q]
    [(struct q:union (r s))
     (make-q:union (top-down f r) (top-down f s))]
    [(struct q:difference (r s))
     (make-q:difference (top-down f r) (top-down f s))]
    [(struct q:intersection (r s))
     (make-q:intersection (top-down f r) (top-down f s))]
    [(struct q:product (r s))
     (make-q:product (top-down f r) (top-down f s))]
    [(struct q:projection (schema r))
     (make-q:projection schema (top-down f r))]
    [(struct q:selection (prop r))
     (make-q:selection prop (top-down f r))]
    [(struct q:rename* (old->new r))
     (make-q:rename* old->new (top-down f r))]))

(define (optimize-query q)
  (bottom-up (compose simplify/once pull-up/once simplify/once)
             (top-down (compose simplify/once push-down/once simplify/once) q)))

(provide/contract
 [optimize-query (query? . -> . query?)]
 [simplify/once (query? . -> . query?)]
 [pull-up/once (query? . -> . query?)]
 [push-down/once (query? . -> . query?)])
