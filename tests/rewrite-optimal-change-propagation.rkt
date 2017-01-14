; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket
(require rackunit)
(require "../racr/core.rkt"
         "../racr/testing.rkt")

(define initialize-ast
  (lambda (cached?)
    (with-specification
     (create-specification)
     (ast-rule 'S->v)
     (compile-ast-specifications 'S)
     (ag-rule
      A
      (S
       cached?
       (lambda (n)
         (if (not (ast-child 'v n))
             #f
             (att-value 'B n)))))
     (ag-rule
      B
      (S
       cached?
       (lambda (n)
         (if (ast-child 'v n)
             #t
             (att-value 'A n)))))
     (compile-ag-specifications)
     (create-ast 'S (list #t)))))

(define run-tests
  (lambda ()
    ; Not cached:
    (let ((ast (initialize-ast #f))) ; A before B
      (check-true (att-value 'A ast))
      (check-true (att-value 'B ast))
      (rewrite-terminal 'v ast #f)
      (check-true (not (att-value 'A ast)))
      (check-true (not (att-value 'B ast))))
    (let ((ast (initialize-ast #f))) ; B before A
      (check-true (att-value 'B ast))
      (check-true (att-value 'A ast))
      (rewrite-terminal 'v ast #f)
      (check-true (not (att-value 'B ast)))
      (check-true (not (att-value 'A ast))))
    
    ; Cached:
    (let ((ast (initialize-ast #t))) ; A before B
      (check-true (att-value 'A ast))
      (check-true (att-value 'B ast))
      (rewrite-terminal 'v ast #f)
      (check-true (not (att-value 'A ast)))
      (check-true (not (att-value 'B ast))))
    (let ((ast (initialize-ast #t))) ; B before A
      (check-true (att-value 'B ast))
      (check-true (att-value 'A ast))
      (rewrite-terminal 'v ast #f)
      (check-true (not (att-value 'B ast)))
      (check-true (not (att-value 'A ast))))))

(run-tests)
