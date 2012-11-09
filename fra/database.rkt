#lang racket/base
(require racket/contract
         racket/match
         "query.rkt"
         "relation.rkt"
         "prop.rkt"
         "tuple.rkt"
         "optimize.rkt")

(define current-database/c
  (-> symbol? relation?))
(define current-database
  (make-parameter
   (lambda (rel-id)
     (error 'current-database "Unknown relation: ~e" rel-id))))

(define (raw-execute-query cache q)
  (hash-ref 
   cache q
   (lambda ()
     (match q
       [(struct q:relation (id))
        ((current-database) id)]
       [(struct q:singleton (schema))
        (singleton-relation schema NULL)]
       [(struct q:union (r s))
        (relation-union (raw-execute-query cache r) (raw-execute-query cache s))]
       [(struct q:difference (r s))
        (relation-difference (raw-execute-query cache r) (raw-execute-query cache s))]
       [(struct q:intersection (r s))
        (relation-intersection (raw-execute-query cache r) (raw-execute-query cache s))]
       [(struct q:product (r s))
        (relation-product (raw-execute-query cache r) (raw-execute-query cache s))]
       [(struct q:projection (schema r))
        (relation-projection schema (raw-execute-query cache r))]
       [(struct q:selection (prop r))
        (define schema (query-schema r))
        (define prop/c (compile-prop prop schema))
        (relation-selection prop/c (raw-execute-query cache r))]
       [(struct q:rename* (old->new r))
        (relation-rename* old->new (raw-execute-query cache r))]))))

(define (execute-query q)
  (define cache (make-hash))
  (define oq (optimize-query q))
  #;(printf "Executing: ~S~n~n" q)
  #;(printf "Optimized: ~S~n~n" oq)
  (raw-execute-query cache oq))

(define database/c 
  (hash/c symbol? relation? #:immutable #t))

(define-syntax-rule (with-database db e ...)
  (call-with-database db (lambda () e ...)))
(define (call-with-database db thnk)
  (parameterize ([current-database
                  (lambda (rel-id)
                    (hash-ref db rel-id))]
                 [current-database-schema
                  (lambda (rel-id)
                    (relation-schema (hash-ref db rel-id)))])
    (thnk)))

(define (database-insert db rel-id tup)
  (define rel-schema
    (relation-schema
     (hash-ref db rel-id
              (lambda () (error 'database-insert "Unknown relation: ~e" rel-id)))))
  (unless (= (length rel-schema) (tuple-length tup))
    (error 'database-insert "Tuple ~a does not match ~a's schema: ~a" tup rel-id rel-schema))
  (hash-update 
   db rel-id
   (lambda (rel) (relation-insert rel tup))))

(define (database-delete db rel-id tup)
  (define rel-schema
    (relation-schema
     (hash-ref db rel-id
              (lambda () (error 'database-delete "Unknown relation: ~e" rel-id)))))
  (unless (= (length rel-schema) (tuple-length tup))
    (error 'database-delete "Tuple ~a does not match ~a's schema: ~a" tup rel-id rel-schema))
  (hash-update 
   db rel-id
   (lambda (rel) (relation-delete rel tup))))

(define-syntax-rule (Database [relation-id schema tuples ...] ...)
  (make-immutable-hasheq
   (list (cons 'relation-id (Relation schema tuples ...))
         ...)))

(provide
 with-database
 Database)
(provide/contract
 [current-database/c contract?]
 [current-database (parameter/c current-database/c)]
 [execute-query (query? . -> . relation?)]
 [database/c contract?]
 [database-insert (database/c symbol? tuple? . -> . database/c)]
 [database-delete (database/c symbol? tuple? . -> . database/c)]
 [call-with-database (database/c (-> any) . -> . any)])
