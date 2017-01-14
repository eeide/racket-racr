; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket
(require rackunit)
(require "../racr/core.rkt"
         "../racr/testing.rkt")

(define initialize-basic-tests
  (lambda (cached?)
    (let ((non-circular-state 0)
          (circular-state 0))
      (with-specification
       (create-specification)
       (ast-rule 'A->)
       (compile-ast-specifications 'A)
       
       (ag-rule
        non-circular
        (A
         cached?
         (lambda (n)
           (set! non-circular-state (+ non-circular-state 1))
           non-circular-state)))
       
       (ag-rule
        circular
        (A
         cached?
         (lambda (n)
           (set! circular-state (+ circular-state 1))
           (if (< (car (att-value 'circular n)) 10)
               (cons (+ (car (att-value 'circular n)) 1) circular-state)
               (cons (car (att-value 'circular n)) circular-state)))
         (cons 0 0)
         (lambda (r1 r2)
           (= (car r1) (car r2)))))
       
       (ag-rule
        cycle-error
        (A
         cached?
         (lambda (n)
           (att-value 'cycle-error n))))
       
       (compile-ag-specifications)
       (create-ast 'A (list))))))

(define initialize-fibonacci-numbers
  (lambda (cached?)
    (with-specification
     (create-specification)
     (ast-rule 'S->)
     (compile-ast-specifications 'S)
     (ag-rule
      fibonacci
      (S
       cached?
       (lambda (n num)
         (if (< num 2)
             num
             (+ (att-value 'fibonacci n (- num 1)) (att-value 'fibonacci n (- num 2)))))))
     (compile-ag-specifications)
     (create-ast 'S (list)))))

(define run-tests
  (lambda ()
    (let ((ast (initialize-basic-tests #f)))
      (check-true (equal? (att-value 'circular ast) (cons 10 10)))
      (check-true (equal? (att-value 'circular ast) (cons 10 21)))
      (check-true (= 1 (att-value 'non-circular ast)))
      (check-true (= 2 (att-value 'non-circular ast)))
      (check-true (equal? (att-value 'circular ast) (cons 10 32)))
      (check-true (equal? (att-value 'circular ast) (cons 10 43)))
      (check-exn exn:fail? (lambda () (att-value 'cycle-error ast))))
    
    (let ((ast (initialize-basic-tests #t)))
      (check-true (equal? (att-value 'circular ast) (cons 10 10)))
      (check-true (equal? (att-value 'circular ast) (cons 10 10)))
      (check-true (= 1 (att-value 'non-circular ast)))
      (check-true (= 1 (att-value 'non-circular ast)))
      (check-true (equal? (att-value 'circular ast) (cons 10 10)))
      (check-true (equal? (att-value 'circular ast) (cons 10 10)))
      (check-exn exn:fail? (lambda () (att-value 'cycle-error ast))))
    
    (letrec ((ast-cached (initialize-fibonacci-numbers #t))
             (ast-not-cached (initialize-fibonacci-numbers #f))
             (fibonacci-naive ; Control Implementation
              (lambda (num)
                (if (< num 2)
                    num
                    (+ (fibonacci-naive (- num 1)) (fibonacci-naive (- num 2)))))))
      (check-true (= (att-value 'fibonacci ast-not-cached 10) (fibonacci-naive 10)))
      (check-true (= (att-value 'fibonacci ast-cached 10) (att-value 'fibonacci ast-not-cached 10)))
      ;(display "Compute Fibonacci Number for n=5000; If it takes a long time, attribute caching is not working:\n")
      ;(display (att-value 'fibonacci ast-cached 5000))
      ;(display "\nFinished computing Fibonacci Number for n=5000.\n")
      )))

(run-tests)
