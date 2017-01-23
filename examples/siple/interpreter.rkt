; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require "../../racr/core.rkt")
(require "type.rkt"
         "state.rkt"
         "exception-api.rkt")
(provide
  weave-interpreter)
 
 (define weave-interpreter
   (lambda (ast)
     (let* ((weave
             (lambda (node-type interpreter-function)
               (ast-weave-annotations
                ast
                node-type
                'interpret
                (lambda (n . args)
                  (unless (att-value 'local-correct? n)
                    (throw-siple-exception "SiPLE Interpreter Error: The program is not well-formed."))
                  (apply interpreter-function n args))))))
       
       (weave
        'CompilationUnit
        (lambda (n output-port)
          (let ((vm (make-state (make-frame #f #f (list) 'siple:nil) output-port)))
            ; Allocate all global variables:
            (ast-for-each-child
             (lambda (i n)
               ((ast-annotation n 'interpret) vm))
             (ast-child 'Declaration* n))
            ; Prepare the main procedure's execution environment:
            (let* ((env-prototype (memory-location-value (state-access vm (att-value 'main-procedure n))))
                   (env-new (make-frame (frame-procedure env-prototype) (frame-closure env-prototype) (list) 'siple:nil))
                   (env-old (state-current-frame vm)))
              (set-state-current-frame! vm env-new)
              ; Execute the main procedure:
              ((ast-annotation (ast-child 'Body (frame-procedure env-new)) 'interpret) vm)
              ; Restore the old execution environment:
              (set-state-current-frame! vm env-old)
              vm))))
       
       (weave
        'ProcedureDeclaration
        (lambda (n s)
          (state-allocate s n (make-frame n (state-current-frame s) (list) 'siple:nil))))
       
       (weave
        'VariableDeclaration
        (lambda (n s)
          (state-allocate s n 'siple:nil)))
       
       (weave
        'Block
        (lambda (n s)
          (call/cc
           (lambda (break)
             (ast-for-each-child
              (lambda (i n)
                ((ast-annotation n 'interpret) s)
                (when (not (eq? (frame-return-value (state-current-frame s)) 'siple:nil))
                    (break)))
              (ast-child 'Statement* n))))))
       
       (weave
        'If
        (lambda (n s)
          (if ((ast-annotation (ast-child 'Condition n) 'interpret) s)
              ((ast-annotation (ast-child 'Body n) 'interpret) s)
              (when (> (ast-num-children (ast-child 'Alternative n)) 0)
                  ((ast-annotation (ast-child 1 (ast-child 'Alternative n)) 'interpret) s)))))
       
       (weave
        'While
        (lambda (n s)
          (call/cc
           (lambda (break)
             (let loop ()
               (if ((ast-annotation (ast-child 'Condition n) 'interpret) s)
                   (begin
                     ((ast-annotation (ast-child 'Body n) 'interpret) s)
                     (if (eq? (frame-return-value (state-current-frame s)) 'siple:nil)
                         (loop)
                         (break)))
                   (break)))))))
       
       (weave
        'VariableAssignment
        (lambda (n s)
          (set-memory-location-value!
           ((ast-annotation (ast-child 'LHand n) 'interpret) s)
           ((ast-annotation (ast-child 'RHand n) 'interpret) s))))
       
       (weave
        'ProcedureReturn
        (lambda (n s)
          (if (> (ast-num-children (ast-child 'Expression* n)) 0)
              (set-frame-return-value! (state-current-frame s) ((ast-annotation (ast-child 1 (ast-child 'Expression* n)) 'interpret) s))
              (set-frame-return-value! (state-current-frame s) (type-undefined)))))
       
       (weave
        'Write
        (lambda (n s)
          (let* ((out-value ((ast-annotation (ast-child 'Expression n) 'interpret) s)))
            (display out-value (state-output-port s))
            (display #\newline (state-output-port s)))))
       
       (weave
        'Read
        (lambda (n s)
          (let ((type (type-rtype (att-value 'type (ast-child 'Expression n))))
                (location ((ast-annotation (ast-child 'Expression n) 'interpret) s)))
            (set-memory-location-value!
             location
             (let loop ((input (read)))
               (cond
                 ((and (type-boolean? type) (boolean? input))
                  input)
                 ((and (type-integer? type) (integer? input))
                  input)
                 ((and (type-real? type) (real? input))
                  input)
                 (else
                  (display (string-append "Unexpected value; Expected [" (type-pretty-print type) "]; Enter valid value!\n"))
                  (loop (read)))))))))
       
       (weave
        'Assertion
        (lambda (n s)
          (unless ((ast-annotation (ast-child 'Expression n) 'interpret) s)
            (throw-siple-exception "Assertion Failed!"))))
       
       (weave
        'Constant
        (lambda (n s)
          (cond
            ((type-boolean? (att-value 'type n))
             (att-value 'as-boolean n))
            ((type-integer? (att-value 'type n))
             (att-value 'as-integer n))
            (else (att-value 'as-real n)))))
       
       (weave
        'Reference
        (lambda (n s)
          (state-access s (att-value 'declaration n))))
       
       (weave
        'ProcedureCall
        (lambda (n s)
          (let* ((args (list))
                 (env-prototype ((ast-annotation (ast-child 'Procedure n) 'interpret) s)) ; Evaluate the procedure operand
                 (env-new (make-frame (frame-procedure env-prototype) (frame-closure env-prototype) (list) 'siple:nil))
                 (env-old (state-current-frame s))
                 (result 'siple:nil))
            ; Evaluate the arguments:
            (ast-for-each-child
             (lambda (i n)
               (set! args (append args (list ((ast-annotation n 'interpret) s)))))
             (ast-child 'Arguments n))
            ; Prepare the execution environment:
            (set-state-current-frame! s env-new)
            (ast-for-each-child
             (lambda (i n)
               (state-allocate s n (list-ref args (- i 1))))
             (ast-child 'Parameters (frame-procedure env-new)))
            ; Execute the procedure:
            ((ast-annotation (ast-child 'Body (frame-procedure env-new)) 'interpret) s)
            (when (not (type-undefined? (att-value 'type n)))
                (set! result (frame-return-value env-new)))
            ; Restore the old execution environment:
            (set-state-current-frame! s env-old)
            result)))
       
       (weave
        'Not
        (lambda (n s)
          (not ((ast-annotation (ast-child 'Operand n) 'interpret) s))))
       
       (weave
        'UMinus
        (lambda (n s)
          (- ((ast-annotation (ast-child 'Operand n) 'interpret) s))))
       
       (weave
        'RealCoercion
        (lambda (n s)
          (+ ((ast-annotation (ast-child 'Operand n) 'interpret) s) 0.0)))
       
       (weave
        'Dereference
        (lambda (n s)
          (let ((value (memory-location-value ((ast-annotation (ast-child 'Operand n) 'interpret) s))))
            (if (eq? value 'siple:nil)
                (raise-user-error
                 'interpret
                 "~s"
                 "SiPLE interpreter exception: Read access to uninitialized entity.")
                value))))
       
       (weave
        'BinaryExpression
        (lambda (n s)
          (let* ((op1 ((ast-annotation (ast-child 'Operand1 n) 'interpret) s))
                 (op2 ((ast-annotation (ast-child 'Operand2 n) 'interpret) s)))
            (case (ast-node-type n)
              ((And) (and op1 op2))
              ((Or) (or op1 op2))
              ((Equal) (eqv? op1 op2))
              ((GreaterThan) (> op1 op2))
              ((LesserThan) (< op1 op2))
              ((GreaterThanEqual) (>= op1 op2))
              ((LesserThanEqual) (<= op1 op2))
              ((Addition) (+ op1 op2))
              ((Subtraction) (- op1 op2))
              ((Multiplication) (* op1 op2))
              ((Division) (/ op1 op2)))))))))
