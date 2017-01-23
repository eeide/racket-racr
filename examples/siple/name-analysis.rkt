; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require "../../racr/core.rkt")
(provide
  specify-name-analysis)
 
 (define specify-name-analysis
   (lambda (siple-specification)
     (with-specification
      siple-specification
      
      (ag-rule
       lookup
       (CompilationUnit
        (lambda (n name)
          (ast-find-child
           (lambda (i n)
             (string=? (ast-child 'name n) name))
           (ast-child 'Declaration* n))))
       
       ((Block Statement*)
        (lambda (n name)
          (let* ((statement-list (ast-parent n))
                 (block (ast-parent statement-list)))
            (or
             (and
              (att-value 'is-procedure-body block)
              (ast-find-child
               (lambda (i n)
                 (string=? (ast-child 'name n) name))
               (ast-child 'Parameters (att-value 'is-procedure-body block))))
             (ast-find-child
              (lambda (i n)
                (and (ast-subtype? n 'Declaration) (string=? (ast-child 'name n) name)))
              statement-list
              (cons 1 (ast-child-index n)))
             (att-value 'lookup block name))))))
      
      (ag-rule
       main-procedure
       (CompilationUnit
        (lambda (n)
          (att-value 'lookup n "main"))))
      
      (ag-rule
       declaration
       (Reference
        (lambda (n)
          (att-value 'lookup n (ast-child 'name n))))))))
