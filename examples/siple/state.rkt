; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require "../../racr/core.rkt")
(provide
  make-state
  state-current-frame
  set-state-current-frame!
  state-output-port
  state-allocate
  state-access
  make-frame
  frame-procedure
  frame-closure
  frame-environment
  set-frame-environment!
  frame-return-value
  set-frame-return-value!
  make-memory-location
  memory-location-value
  set-memory-location-value!)
 
 (struct state
   ((current-frame #:mutable)
    output-port)
   #:constructor-name make-state)
 
 (define state-allocate
   (lambda (state decl value)
     (let* ((env (frame-environment (state-current-frame state)))
            (entry (assq decl env)))
       (if entry
           (set-memory-location-value! (cdr entry) value)
           (set-frame-environment! (state-current-frame state) (cons (cons decl (make-memory-location value)) env))))))
 
 (define state-access
   (lambda (state decl)
     (let loop ((frame (state-current-frame state)))
       (let ((entity? (assq decl (frame-environment frame))))
         (if entity?
             (cdr entity?)
             (loop (frame-closure frame)))))))
 
 (struct frame
   (procedure
    closure
    (environment #:mutable)
    (return-value #:mutable))
   #:constructor-name make-frame)
 
 (struct memory-location
   ((value #:mutable))
   #:constructor-name make-memory-location)
