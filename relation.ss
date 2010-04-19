#lang scheme
(require (planet dherman/set:4/set)
         "schema.ss"
         "tuple.ss"
         "prop.ss")

; XXX Remove schema?
(define-struct relation (schema tuples)
  #:transparent
  #:property prop:equal+hash
  (list (match-lambda*
          [(list (struct relation (a1 t1))
                 (struct relation (a2 t2))
                 rec-equal?)
           (and (rec-equal? a1 a2) (set=? t1 t2))])
        (lambda (r1 rec-hash-proc) 
          (+ (rec-hash-proc (relation-schema r1))
             (rec-hash-proc (relation-tuples r1))))
        (lambda (r1 rec-hash2-proc) 
          (+ (rec-hash2-proc (relation-schema r1))
             (rec-hash2-proc (relation-tuples r1))))))

(define (singleton-relation schema elem)
  (make-relation 
   schema
   (set-add (singleton-tuple (schema-length schema) elem)
            empty-set)))

(define (prim-lift prim)
  (define lifted
    (match-lambda*
      [(list (and R1 (struct relation (S1 T1)))
             (and R2 (struct relation (S2 T2))))
       (cond
         [(equal? S1 S2)
          (make-relation S1 (prim T1 T2))]
         [(schema-orderi-equal? S1 S2)
          (lifted R1 (relation-projection S1 R2))]
         [else
          (error 'relation-union-compatible 
                 "Relations must be union-compatible: ~S and ~S"
                 S1 S2)])]))
  lifted)

(define relation-union (prim-lift set-union))
(define relation-difference (prim-lift set-difference))
(define relation-intersection (prim-lift set-intersection))

(define relation-product
  (match-lambda*
    [(list (struct relation (schema1 tuples1))
           (struct relation (schema2 tuples2)))
     (unless (schema-disjoint? schema1 schema2)
       (error 'cross-join "Relations must have disjoint headers"))
     (make-relation
      (schema-disjoint-union schema1 schema2)
      (for*/set ([tup1 (in-set tuples1)]
                 [tup2 (in-set tuples2)])
                (tuple-disjoint-union tup1 tup2)))]))

(define (relation-projection new-schema rel)
  (match rel
    [(struct relation (old-schema tuples))
     (define schema-proj
       (for/list ([a (in-list new-schema)])
         (schema-ref old-schema a)))
     (make-relation
      new-schema
      (for/set ([tup (in-set tuples)])
               (tuple-proj schema-proj tup)))]))

(define (relation-selection psi/c rel)
  (match rel
    [(struct relation (schema tuples))
     (struct-copy relation rel
                  [tuples
                   (for/set ([tup (in-set tuples)]
                             #:when (prop-holds? psi/c tup))
                            tup)])]))

(define (relation-rename attr1 attr2 rel)
  (relation-rename* (make-immutable-hasheq (list (cons attr1 attr2))) rel))

(define (relation-rename* renaming rel)
  (match rel
    [(struct relation (schema tups))
     (make-relation (schema-replace* renaming schema) tups)]))

(define-struct the-NULL () #:prefab)
(define NULL (make-the-NULL))

(define-syntax Relation
  (syntax-rules ()
    [(_ (attribute ...)
        [value ...] ...)
     ; XXX Error checking of tuple lengths
     (make-relation (schema 'attribute ...)
                    (list->set 
                     (list 
                      (Tuple value ...)
                      ...)))]))

(define (relation-insert old tuple)
  (struct-copy relation old
               [tuples (set-add tuple (relation-tuples old))]))

(define (set-remove elem set)
  (set-difference set (set-add elem empty-set)))
  
(define (relation-delete old tuple)
  (struct-copy relation old
               [tuples (set-remove tuple (relation-tuples old))]))

(provide/contract
 [relation? (any/c . -> . boolean?)]
 [relation-schema (relation? . -> . schema/c)]
 [relation-tuples (relation? . -> . set?)]
 [singleton-relation (schema/c any/c . -> . relation?)]
 [relation-union (relation? relation? . -> . relation?)]
 [relation-difference (relation? relation? . -> . relation?)]
 [relation-intersection (relation? relation? . -> . relation?)]
 [relation-product (relation? relation? . -> . relation?)]
 [relation-projection (schema/c relation? . -> . relation?)]
 [relation-selection (compiled-prop/c relation? . -> . relation?)]
 [relation-rename (symbol? symbol? relation? . -> . relation?)]
 [relation-rename* ((hash/c symbol? symbol?) relation? . -> . relation?)]
 [relation-insert (relation? tuple? . -> . relation?)]
 [relation-delete (relation? tuple? . -> . relation?)]
 [NULL any/c])
(provide
 Relation)