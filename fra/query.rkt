#lang racket/base
(require racket/match
         racket/contract
         racket/dict
         "schema.rkt"
         "prop.rkt")

; Data Structures & Contracts
(define (query-print q port write?)
  (match q
    [(struct q:relation (id))
     (fprintf port "~a" id)]
    [(struct q:singleton (schema))
     (fprintf port "[singleton]")]
    [(struct q:union (r s))
     (fprintf port "(~a ∪ ~a)" r s)]
    [(struct q:difference (r s))
     (fprintf port "(~a - ~a)" r s)]
    [(struct q:intersection (r s))
     (fprintf port "(~a ∩ ~a)" r s)]
    [(struct q:product (r s))
     (fprintf port "(~a ✕ ~a)" r s)]
    [(struct q:projection (schema r))
     (fprintf port "π_~a(~a)" schema r)]
    [(struct q:selection (prop r))
     (fprintf port "σ_[~a](~a)" prop r)]
    [(struct q:rename* (old->new r))
     (fprintf port "ρ_[~a](~a)" (hash-map old->new (lambda (k v) (format "~a/~a" k v))) r)]))

(define-struct query () #:transparent #:property prop:custom-write query-print)
(define-struct (q:relation query) (id) #:transparent)
(define-struct (q:singleton query) (schema) #:transparent)
(define-struct (q:union query) (r s) #:transparent)
(define-struct (q:difference query) (r s) #:transparent)
(define-struct (q:intersection query) (r s) #:transparent)
(define-struct (q:product query) (r s) #:transparent)
(define-struct (q:projection query) (schema r) #:transparent)
(define-struct (q:selection query) (prop r) #:transparent)
(define-struct (q:rename* query) (old->new r) #:transparent)

; Helpers
(define database-schema/c
  (-> symbol? schema/c))

(define current-database-schema
  (make-parameter
   (lambda (rel-id)
     (error 'current-database-schema "Unknown relation: ~e" rel-id))))
(define (query-schema q)
  (match q
    [(struct q:relation (id))
     ((current-database-schema) id)]
    [(struct q:singleton (schema))
     schema]
    [(struct q:union (r s))
     (query-schema r)]
    [(struct q:difference (r s))
     (query-schema r)]
    [(struct q:intersection (r s))
     (query-schema r)]
    [(struct q:product (r s))
     (schema-disjoint-union 
      (query-schema r)
      (query-schema s))]
    [(struct q:projection (schema r))
     schema]
    [(struct q:selection (prop r))
     (query-schema r)]
    [(struct q:rename* (old->new r))
     (schema-replace* 
      old->new
      (query-schema r))]))

; Synthetic
(define (query-relation id)
  (unless ((current-database-schema) id)
    (error 'query-relation "Relation does not exist in schema: ~e" id))
  (make-q:relation id))

(define (query-singleton schema)
  (make-q:singleton schema))

; Primitives
(define (union-compatible make-query)
  (lambda (R S)
    (define R-schema (query-schema R))
    (define S-schema (query-schema S))
    (cond
      [(equal? R-schema S-schema)
       (make-query R S)]
      [(schema-orderi-equal? R-schema S-schema)
       (make-query R (query-projection R-schema S))]
      [else
       (error 'union-compatible 
              "Queries must be union-compatible: ~S and ~S"
              R-schema S-schema)])))

(define query-union (union-compatible make-q:union))
(define query-difference (union-compatible make-q:difference))
(define query-intersection (union-compatible make-q:intersection))

(define (query-product R S)
  (define R-schema (query-schema R))
  (define S-schema (query-schema S))
  (unless (schema-disjoint? R-schema S-schema)
    (error 'product "Relations must have disjoint headers"))
  (make-q:product R S))

(define (query-projection new-schema R)
  (unless (schema-subset? new-schema (query-schema R))
    (error 'query-projection "Schema must be a subset: ~e" new-schema))
  (make-q:projection new-schema R))

(define (query-selection psi R)
  (unless (schema-subset? (prop-schema psi) (query-schema R))
    (error 'query-selection "Proposition must refer to subset of query's attributes: ~e" psi))
  (make-q:selection psi R))

(define (query-rename attr1 attr2 rel)
  (query-rename* (make-immutable-hasheq (list (cons attr1 attr2))) rel))

(define (query-rename* renaming R)
  (unless (schema-valid-renaming? renaming (query-schema R))
    (error 'query-rename* "Invalid renaming: ~e" renaming))
  (make-q:rename* renaming R))

; Derived
(define (query-natural-join R S [equal? equal?])
  (define R-schema (query-schema R))
  (define S-schema (query-schema S))
  ; Find the common attributes
  (define common-schema (schema-intersection R-schema S-schema))
  ; Computed new identifiers
  (define renaming
    (for/hasheq ([common (in-list common-schema)])
      (values common (gensym common))))
  ; Compute the prop of equality
  (define common-equal-prop (equality-prop renaming equal?))
  ; Rename common things on one side
  (define renamed-R (query-rename* renaming R))
  ; Compute the combined table
  (define combined (query-product renamed-R S))
  ; Select the ones that matter
  (define joined (query-selection common-equal-prop combined))
  ; Removed the renamed attributes
  (define final (query-projection (schema-union R-schema S-schema) joined))
  final)

(define (query-theta-join theta R S)
  (query-selection theta (query-product R S)))

(define (query-semi-join R S)
  (query-projection (query-schema R) (query-natural-join R S)))

(define (query-anti-join R S)
  (query-difference R (query-semi-join R S)))

(define (query-division R S)
  (define R-schema (query-schema R))
  (define S-schema (query-schema S))
  (define R-unique-schema (schema-difference R-schema S-schema))
  (define R/unique (query-projection R-unique-schema R))
  (define T (query-product R/unique S))
  (define U (query-difference T R))
  (define V (query-projection R-unique-schema U))
  (define W (query-difference R/unique V))
  W)

(define (query-left-outer-join R S)
  (define R-schema (query-schema R))
  (define S-schema (query-schema S))
  (define R<>S (query-natural-join R S))
  (define S-unique-schema (schema-difference S-schema R-schema))
  (query-union
   R<>S
   (query-product
    (query-difference
     R
     (query-projection R-schema R<>S))
    (query-singleton S-unique-schema))))

(define (query-right-outer-join R S)
  (define R-schema (query-schema R))
  (define S-schema (query-schema S))
  (define R<>S (query-natural-join R S))
  (define R-unique-schema (schema-difference R-schema S-schema))
  (query-union
   R<>S
   (query-product
    (query-difference
     S
     (query-projection S-schema R<>S))
    (query-singleton R-unique-schema))))

(define (query-outer-join R S)
  (query-union 
   (query-left-outer-join R S)
   (query-right-outer-join R S)))

; Provides
(provide/contract
 ; Structs
 [struct query ()]
 [struct (q:relation query) ([id symbol?])]
 [struct (q:singleton query) ([schema schema/c])]
 [struct (q:union query) ([r query?] [s query?])]
 [struct (q:difference query) ([r query?] [s query?])]
 [struct (q:intersection query) ([r query?] [s query?])]
 [struct (q:product query) ([r query?] [s query?])]
 [struct (q:projection query) ([schema schema/c] [r query?])]
 [struct (q:selection query) ([prop prop?] [r query?])]
 [struct (q:rename* query) ([old->new dict?] [r query?])]
 ; Helpres
 [database-schema/c contract?]
 [current-database-schema (parameter/c database-schema/c)]
 [query-schema (query? . -> . schema/c)]
 ; Queries
 [query-relation (symbol? . -> . query?)]
 [query-union (query? query? . -> . query?)]
 [query-difference (query? query? . -> . query?)]
 [query-intersection (query? query? . -> . query?)]
 [query-product (query? query? . -> . query?)]
 [query-projection (schema/c query? . -> . query?)]
 [query-selection (prop? query? . -> . query?)]
 [query-rename (symbol? symbol? query? . -> . query?)]
 [query-rename* ((hash/c symbol? symbol?) query? . -> . query?)]
 [query-natural-join ((query? query?) ((any/c any/c . -> . boolean?)) . ->* . query?)]
 [query-theta-join (prop? query? query? . -> . query?)]
 [query-semi-join (query? query? . -> . query?)]
 [query-anti-join (query? query? . -> . query?)]
 [query-division (query? query? . -> . query?)]
 [query-left-outer-join (query? query? . -> . query?)]
 [query-right-outer-join (query? query? . -> . query?)]
 [query-outer-join (query? query? . -> . query?)])
