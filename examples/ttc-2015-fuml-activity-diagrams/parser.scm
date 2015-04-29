; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-fuml-activity-diagrams parser)
 (export parse-diagram parse-diagram-input)
 (import (rnrs) (ttc-2015-fuml-activity-diagrams language))
 
 (define (parse-diagram file)
   (with-input-from-file file parse-activity))
 
 (define (parse-diagram-input file)
   (with-input-from-file file
     (lambda ()
       (define (parse-input)
         (define name (parse-identifier))
         (parse-keyword "=")
         (:Variable name #f (parse-constant)))
       (parse-list parse-input))))
 
 (define p-char ; Return the next character if it satisfies optional constraints and exists.
   (lambda constraints
     (define current-char (peek-char))
     (and (not (eof-object? current-char))
          (for-all (lambda (f) (f current-char)) constraints)
          current-char)))
 
 (define r-char ; Similar to p-char but additionally increments the parsing position.
   (lambda constraints
     (unless (apply p-char constraints) (exception: "Parsing Error"))
     (read-char)))
 
 (define (char= to-read) ; Construct filter for certain character that can be used by p- and r-char.
   (lambda (char-read)
     (char=? char-read to-read)))
 
 (define (consume-whitespace)
   (when (p-char char-whitespace?) (r-char) (consume-whitespace))
   (when (p-char (char= #\/))
     (r-char)
     (r-char (char= #\/))
     (let loop ()
       (if (p-char (char= #\newline))
           (r-char)
           (begin (r-char) (loop))))
     (consume-whitespace)))
 
 (define (parse-boolean)
   (cond ((p-char (char= #\f)) (parse-keyword "false") #f)
         (else (parse-keyword "true") #t)))
 
 (define (parse-integer) ; Read sequence of digits.
   (define num
     (let loop ((num (list (r-char char-numeric?))))
       (let ((next-char? (p-char)))
         (if (and next-char? (char-numeric? next-char?))
             (loop (cons (r-char) num))
             (string->number (apply string (reverse num)))))))
   (consume-whitespace)
   num)
 
 (define (parse-identifier) ; Parse ordinary identifier, i.e., [a-zA-Z][_a-zA-Z0-9]*.
   (define id
     (let loop ((id (list (r-char char-alphabetic?))))
       (let ((next-char? (p-char)))
         (if (and next-char?
                  (or (char=? next-char? #\_)
                      (char-alphabetic? next-char?)
                      (char-numeric? next-char?)))
             (loop (cons (r-char) id))
             (string->symbol (apply string (reverse id)))))))
   (consume-whitespace)
   id)
 
 (define (parse-constant)
   (if (p-char char-numeric?) (parse-integer) (parse-boolean)))
 
 (define (parse-keyword keyword)
   (string-for-each (lambda (c) (r-char (char= c))) keyword)
   (consume-whitespace))
 
 (define (parse-list f)
   (define elem (f))
   (cond ((p-char (char= #\,)) (parse-keyword ",") (cons elem (parse-list f)))
         (else (list elem))))
 
 (define (parse-activity)
   (define name #f)(define Variable* (list))(define ActivityNode* (list))(define ActivityEdge* (list))
   (consume-whitespace)
   (parse-keyword "activity")
   (set! name (parse-identifier))
   (when (p-char (char= #\())
     (parse-keyword "(")
     (unless (p-char (char= #\)))
       (set! Variable* (append Variable* (parse-list parse-input))))
     (parse-keyword ")"))
   (parse-keyword "{")
   (unless (p-char (char= #\n))
     (set! Variable* (append Variable* (parse-list parse-local))))
   (parse-keyword "nodes")
   (parse-keyword "{")
   (unless (p-char (char= #\}))
     (set! ActivityNode* (parse-list parse-node)))
   (parse-keyword "}")
   (parse-keyword "edges")
   (parse-keyword "{")
   (unless (p-char (char= #\}))
     (set! ActivityEdge* (parse-list parse-edge)))
   (parse-keyword "}")
   (parse-keyword "}")
   (when (p-char) (exception: "Parsing Error"))
   (:Activity name Variable* ActivityNode* ActivityEdge*))
 
 (define (parse-input)
   (define type (parse-type))
   (:Variable (parse-identifier) type (list)))
 
 (define (parse-local)
   (define type (parse-type))
   (define name (parse-identifier))
   (parse-keyword "=")
   (:Variable name type (parse-constant)))
 
 (define (parse-type)
   (cond ((p-char (char= #\b)) (parse-keyword "bool") Boolean)
         (else (parse-keyword "int") Integer)))
 
 (define (parse-node)
   (define name #f)
   (define expressions (list))
   (define type
     (cond
       ((p-char (char= #\i))
        (parse-keyword "initial")
        :InitialNode)
       ((p-char (char= #\f))
        (r-char)
        (cond
          ((p-char (char= #\i))
           (parse-keyword "inal")
           :FinalNode)
          (else
           (parse-keyword "ork")
           :ForkNode)))
       ((p-char (char= #\j))
        (parse-keyword "join")
        :JoinNode)
       ((p-char (char= #\d))
        (parse-keyword "decision")
        :DecisionNode)
       ((p-char (char= #\m))
        (parse-keyword "merge")
        :MergeNode)
       ((p-char (char= #\a))
        (parse-keyword "action")
        :ExecutableNode)
       (else (exception: "Parsing Error"))))
   (set! name (parse-identifier))
   (when (and (eq? type :ExecutableNode) (p-char (char= #\c)))
     (parse-keyword "comp")
     (parse-keyword "{")
     (set! expressions (parse-list parse-expression))
     (parse-keyword "}"))
   (consume-edges)
   (if (eq? type :ExecutableNode)
       (type name expressions)
       (type name)))
 
 (define (parse-expression)
   (define asignee (parse-identifier))
   (parse-keyword "=")
   (if (p-char (char= #\!))
       (begin (parse-keyword "!") (:UnaryExpression asignee not (parse-identifier)))
       (let* ((operand1 (parse-identifier))
              (operator (parse-binary-operator)))
         (:BinaryExpression asignee operator operand1 (parse-identifier)))))
 
 (define (parse-binary-operator)
   (cond
     ((p-char (char= #\+))
      (parse-keyword "+") +)
     ((p-char (char= #\-))
      (parse-keyword "-") -)
     ((p-char (char= #\<))
      (r-char)
      (if (p-char (char= #\=))
          (begin (parse-keyword "=") <=)
          (begin (consume-whitespace) <)))
     ((p-char (char= #\=))
      (parse-keyword "==") =)
     ((p-char (char= #\>))
      (r-char)
      (if (p-char (char= #\=))
          (begin (parse-keyword "=") >=)
          (begin (consume-whitespace) >)))
     ((p-char (char= #\&))
      (parse-keyword "&") &&)
     ((p-char (char= #\|))
      (parse-keyword "|") //)))
 
 (define (consume-edges)
   (when (p-char (char= #\i))
     (parse-keyword "in")
     (parse-keyword "(")
     (parse-list parse-identifier)
     (parse-keyword ")"))
   (when (p-char (char= #\o))
     (parse-keyword "out")
     (parse-keyword "(")
     (parse-list parse-identifier)
     (parse-keyword ")")))
 
 (define (parse-edge)
   (define name #f)(define source #f)(define target #f)(define guard #f)
   (parse-keyword "flow")
   (set! name (parse-identifier))
   (parse-keyword "from")
   (set! source (parse-identifier))
   (parse-keyword "to")
   (set! target (parse-identifier))
   (when (p-char (char= #\[))
     (parse-keyword "[")
     (set! guard (parse-identifier))
     (parse-keyword "]"))
   (if guard (:ControlFlow source target guard) (:ActivityEdge source target))))