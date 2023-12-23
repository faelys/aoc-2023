; Copyright (c) 2023, Natacha Porté
;
; Permission to use, copy, modify, and distribute this software for any
; purpose with or without fee is hereby granted, provided that the above
; copyright notice and this permission notice appear in all copies.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(import (chicken io) (chicken sort) (chicken string)
        comparse
        trace
        srfi-1
        srfi-14
        srfi-69)

;;;;;;;;;;;;;;;;;
;; Input parsing

(define letters
  (as-string (one-or-more (in char-set:letter))))

(define operator
  (in #\% #\&))

(define source
  (any-of (as-string (char-seq "broadcaster"))
          (sequence operator letters)))

(define data-line
  (sequence* ((label source)
              (_ (char-seq " -> "))
              (first letters)
              (rest (zero-or-more (preceded-by (char-seq ", ") letters)))
              (_ (is #\newline)))
    (result `(,label ,first . ,rest))))

(define all-data
  (one-or-more data-line))

(define data
   (map (lambda (line) (if (pair? (car line))
                           (cons (cadar line) (cons (caar line) (cdr line)))
                           line))
        (parse all-data (read-string))))
(define verbose (< (length data) 10))
(when verbose (write-line (conc "Input: " data)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define node-names (map car data))

(define node-hash (alist->hash-table data))

(define back-links
  (let ((result (alist->hash-table (map (lambda (name) (list name))
                                        node-names))))
    (let outer-loop ((todo data))
      (if (null? todo)
          result
          (begin
            (let inner-loop ((name (caar todo))
                             (dest (if (equal? (caar todo) "broadcaster")
                                       (cdar todo)
                                       (cddar todo))))
              (unless (null? dest)
                (hash-table-set! result (car dest)
;                    (cons name (hash-table-ref result (car dest))))
                     (cons name (hash-table-ref/default result (car dest) '())))
                (inner-loop name (cdr dest))))
            (outer-loop (cdr todo)))))))

(define memories
  (alist->hash-table
    (filter (lambda (line) (not (null? line)))
            (map (lambda (line)
                   (cond ((eqv? (cadr line) #\&)
                             (cons (car line)
                                   (alist->hash-table
                                     (map (lambda (dest) (cons dest #f))
                                          (hash-table-ref back-links
                                                          (car line))))))
                         ((eqv? (cadr line) #\%) (cons (car line) #f))
                         (else '())))
                 data))))

(define (all? hash)
  (let loop ((todo (hash-table->alist hash)))
    (cond ((null? todo) #t)
          ((not (cdar todo)) #f)
          (else (loop (cdr todo))))))

(define (run-node from high? name)
(if (hash-table-exists? node-hash name)
  (let ((def (hash-table-ref node-hash name)))
    (cond ((eqv? (car def) #\%)
             (if high? '()
                 (let ((new-state (not (hash-table-ref memories name))))
                   (hash-table-set! memories name new-state)
                   (map (lambda (dest) (list name new-state dest)) (cdr def)))))
          ((eqv? (car def) #\&)
             (let ((mem (hash-table-ref memories name)))
               (hash-table-set! mem from high?)
               (let ((sent-state (not (all? mem))))
                 (map (lambda (dest) (list name sent-state dest)) (cdr def)))))
          (else (assert #f "Unrunnable def " def " at node " name))))
'()))

(define (run-list state-list)
  (apply append (map (lambda (args) (apply run-node args)) state-list)))

(define start-list
  (map (lambda (dest) (list "broadcaster" #f dest))
       (hash-table-ref node-hash "broadcaster")))

(define (run-cycle state-list n-low n-high)
  (if (null? state-list)
      (list n-low n-high)
      (run-cycle (run-list state-list)
                 (+ n-low (apply + (map (lambda (line) (if (cadr line) 0 1)) state-list)))
                 (+ n-high (apply + (map (lambda (line) (if (cadr line) 1 0)) state-list))))))

(define (run-cycles n n-low n-high)
  (if (= 0 n)
      (list n-low n-high)
      (let ((c (run-cycle start-list (add1 n-low) n-high)))
        (run-cycles (sub1 n) (car c) (cadr c)))))
;(trace run-list)

(write-line (conc "First puzzle:  " (apply * (run-cycles 1000 0 0))))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

; The second puzzle was done with eyes and paper, after having the spoiler
; that the problem is supposed to be solved using the special structure of
; the invidual input.
; I hate this kind of puzzles.

;(define memories
;  (alist->hash-table
;    (filter (lambda (line) (not (null? line)))
;            (map (lambda (line)
;                   (cond ((eqv? (cadr line) #\&)
;                             (cons (car line)
;                                   (alist->hash-table
;                                     (map (lambda (dest) (cons dest #f))
;                                          (hash-table-ref back-links
;                                                          (car line))))))
;                         ((eqv? (cadr line) #\%) (cons (car line) #f))
;                         (else '())))
;                 data))))
;(define (run-cycle-2 state-list n-low n-high n-rx)
;  (if (null? state-list)
;      (list n-low n-high n-rx)
;      (run-cycle-2 (run-list state-list)
;                   (+ n-low (apply + (map (lambda (line) (if (cadr line) 0 1)) state-list)))
;                   (+ n-high (apply + (map (lambda (line) (if (cadr line) 1 0)) state-list)))
;                   (+ n-rx (apply + (map (lambda (line) (if (and (not (cadr line)) (equal? (caddr line) "ln")) 1 0)) state-list)))
;)))

;(define (run-cycles-2 n n-low n-high)
;      (let ((c (run-cycle-2 start-list (add1 n-low) n-high 0)))
;(if (> (caddr c) 0)
;(write-line (conc n " " (caddr c)))
;        (run-cycles-2 (add1 n) (car c) (cadr c)))))

;(write-line (conc "Second puzzle: " (run-cycles-2 1 0 0)))
