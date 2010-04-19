#lang scheme
(require "schema.ss"
         "prop.ss"
         "query.ss"
         "tuple.ss"
         "relation.ss"
         "database.ss")

(provide
 ;; schema.ss
 schema/c
 ;; prop.ss
 prop?
 make-prop:or
 make-prop:and
 make-prop:not
 make-prop:op
 Proposition
 ;; query.ss
 query?
 database-schema/c
 current-database-schema
 query-schema
 query-relation
 query-union
 query-difference
 query-intersection
 query-product
 query-projection
 query-selection
 query-rename
 query-rename*
 query-natural-join
 query-theta-join
 query-semi-join
 query-anti-join
 query-division
 query-left-outer-join
 query-right-outer-join
 query-outer-join
 ;; tuple.ss
 tuple?
 tuple
 tuple-length
 tuple-ref
 Tuple
 ;; relation
 relation?
 relation-schema
 relation-tuples
 NULL
 Relation
 ;; database.ss
 with-database
 Database
 call-with-database
 database/c
 database-insert
 database-delete
 execute-query)