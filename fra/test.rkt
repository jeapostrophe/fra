#lang racket/base
(require tests/eli-tester
         "main.rkt"
         "ivector.rkt")

(define TestDB
  (Database
   [Students2
    (Student Task)
    ('Fred 'Database1)
    ('Fred 'Database2)
    ('Fred 'Compiler1)
    ('Eugene 'Database1)
    ('Eugene 'Compiler1)
    ('Sara 'Database1)
    ('Sara 'Database2)]
   [Tasks2
    (Task)
    ('Database1)
    ('Database2)]
   [Person
    (Name Age Weight)
    ("Harry" 34 80)
    ("Sally" 28 64)
    ("George" 29 70)
    ("Helena" 54 54)
    ("Peter" 34 80)]
   [Employees
    (Name EmployeeId)
    ("Harry" 3415)
    ("Sally" 2241)]
   [Employee
    (Name EmpId DeptName)
    ("Harry" 3415 "Finance")
    ("Sally" 2241 "Sales")
    ("George" 3401 "Finance")
    ("Harriet" 2202 "Sales")]
   [Dept
    (DeptName Manager)
    ("Finance" "George")
    ("Sales" "Harriet")
    ("Production" "Charles")]
   [Employee2
    (Name EmpId DeptName)
    ('Harry 3415 'Finance)
    ('Sally 2241 'Sales)
    ('George 3401 'Finance)
    ('Harriet 2202 'Sales)
    ('Tim 1123 'Executive)]
   [Dept2 (DeptName Manager)
          ('Sales 'Harriet)
          ('Production 'Charles)]
   [Cars
    (CarModel CarPrice)
    ("A" 20)
    ("B" 30)
    ("C" 50)]
   [Boats
    (BoatModel BoatPrice)
    ("A" 10)
    ("B" 40)
    ("C" 60)]))

(with-database 
 TestDB
 (test
  (ivector-union (ivector 1 2) (ivector 3 4)) => (ivector 1 2 3 4)
  ((ivector-union (ivector 1 2) (ivector 3 4)) . equal? . (ivector 1 2 3 4))
  
  (execute-query (query-union (query-relation 'Employee) (query-relation 'Dept))) =error> "compatible"
  
  (execute-query (query-projection '(Age Weight) (query-relation 'Person))) =>
  (Relation (Age Weight)
            (34 80)
            (28 64)
            (29 70)
            (54 54))
  
  (execute-query (query-selection (Proposition ((lambda (age) (>= age 34)) Age))
                                  (query-relation 'Person)))
  =>
  (Relation (Name Age Weight)
            ("Harry" 34 80)
            ("Helena" 54 54)
            ("Peter" 34 80))
  
  (execute-query (query-selection (Proposition (= Age Weight))
                                  (query-relation 'Person)))
  =>
  (Relation (Name Age Weight)
            ("Helena" 54 54))
  
  (execute-query (query-rename 'Name 'EmployeeName
                               (query-relation 'Employees)))
  =>
  (Relation (EmployeeName EmployeeId)
            ("Harry" 3415)
            ("Sally" 2241))
  
  (execute-query (query-projection '(Name EmpId DeptName Manager)
                                   (query-natural-join (query-relation 'Employee)
                                                       (query-relation 'Dept))))
  =>
  (Relation (Name EmpId DeptName Manager)
            ("Harry" 3415 "Finance" "George")
            ("Sally" 2241 "Sales" "Harriet")
            ("George" 3401 "Finance" "George")
            ("Harriet" 2202 "Sales" "Harriet"))
  
  (execute-query (query-projection '(CarModel CarPrice BoatModel BoatPrice)
                                   (query-theta-join (Proposition (>= CarPrice BoatPrice))
                                                     (query-relation 'Cars) (query-relation 'Boats))))
  =>
  (Relation (CarModel CarPrice BoatModel BoatPrice)
            ("A" 20 "A" 10)
            ("B" 30 "A" 10)
            ("C" 50 "A" 10)
            ("C" 50 "B" 40))
  
  (execute-query (query-projection '(Name EmpId DeptName)
                                   (query-semi-join (query-relation 'Employee) (query-relation 'Dept))))
  =>
  (Relation (Name EmpId DeptName)
            ("Harry" 3415 "Finance")
            ("Sally" 2241 "Sales")
            ("George" 3401 "Finance")
            ("Harriet" 2202 "Sales"))
  
  (execute-query (query-projection '(Name EmpId DeptName)
                                   (query-anti-join (query-relation 'Employee) (query-relation 'Dept))))
  =>
  (Relation (Name EmpId DeptName))
  
  (execute-query (query-division (query-relation 'Students2)
                                 (query-relation 'Tasks2)))
  =>
  (Relation (Student)
            ('Fred)
            ('Sara))
  
  (execute-query (query-projection '(Name EmpId DeptName Manager)
                                   (query-left-outer-join (query-relation 'Employee2)
                                                          (query-relation 'Dept2))))
  =>
  (Relation (Name EmpId DeptName Manager)
            ('Harry 3415 'Finance NULL)
            ('Sally 2241 'Sales 'Harriet)
            ('George 3401 'Finance NULL)
            ('Harriet 2202 'Sales 'Harriet)
            ('Tim 1123 'Executive NULL))
  
  (execute-query (query-projection '(Name EmpId DeptName Manager)
                                   (query-right-outer-join (query-relation 'Employee2)
                                                           (query-relation 'Dept2))))
  =>
  (Relation (Name EmpId DeptName Manager)
            ('Sally 2241 'Sales 'Harriet)
            ('Harriet 2202 'Sales 'Harriet)
            (NULL NULL 'Production 'Charles))
  
  (execute-query (query-projection '(Name EmpId DeptName Manager)
                                   (query-outer-join (query-relation 'Employee2)
                                                     (query-relation 'Dept2))))
  =>
  (Relation (Name EmpId DeptName Manager)
            ('Sally 2241 'Sales 'Harriet)
            ('Harriet 2202 'Sales 'Harriet)
            (NULL NULL 'Production 'Charles)
            ('Harry 3415 'Finance NULL)
            ('George 3401 'Finance NULL)
            ('Tim 1123 'Executive NULL))))

; Database Insert
(test
 (with-database TestDB
                (execute-query (query-relation 'Tasks2))) 
 =>
 (Relation (Task)
           ('Database1)
           ('Database2))
 
 (with-database (database-insert TestDB 'Tasks2 (Tuple 'Database3))
                (execute-query (query-relation 'Tasks2)))
 =>
 (Relation (Task)
           ('Database1)
           ('Database2)
           ('Database3))
 
 (with-database (database-delete TestDB 'Tasks2 (Tuple 'Database3))
                (execute-query (query-relation 'Tasks2)))
 =>
 (Relation (Task)
           ('Database1)
           ('Database2))
 
 (with-database (database-delete TestDB 'Tasks2 (Tuple 'Database2))
                (execute-query (query-relation 'Tasks2)))
 =>
 (Relation (Task)
           ('Database1)))
