; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(provide
  throw-siple-exception
  #;siple-exception?)
 
 #;(define-condition-type siple-exception
   &violation
   make-siple-exception
   siple-exception?)
 
 (define throw-siple-exception
   (lambda (message)
     (raise-user-error 'siple "~s" message)))
