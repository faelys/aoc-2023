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

(import (chicken io) (chicken string)
        comparse
        srfi-1
        srfi-14
        srfi-69)

;;;;;;;;;;;;;;;;;
;; Input parsing

(define (as-number parser)
  (bind (as-string parser)
        (lambda (s)
          (result (string->number s)))))

(define spaces
  (one-or-more (is #\space)))

(define digit
  (in char-set:digit))

(define digits
  (as-number (one-or-more digit)))

(define number-list
  (sequence* ((first digits)
              (rest (zero-or-more (preceded-by (is #\,) digits))))
    (result (cons first rest))))

(define symbol
  (in #\. #\# #\?))

(define line
  (sequence* ((symbols (zero-or-more symbol))
              (_ spaces)
              (numbers number-list)
              (_ (is #\newline)))
    (result (list symbols numbers))))

(define all-data
  (zero-or-more line))

(define data (parse all-data (read-string)))
;(write-line (conc "Input: " data))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (count-broken l)
  (let loop ((todo l) (acc '(0)))
     (if (null? todo)
         (reverse (if (= (car acc) 0) (cdr acc) acc))
         (loop (cdr todo)
               (cond ((eqv? (car todo) #\#) (cons (add1 (car acc)) (cdr acc)))
                     ((= (car acc) 0) acc)
                     (else (cons 0 acc)))))))

;(for-each (lambda (line) (write-line (conc (car line) " -> " (count-broken (car line))))) data)

(define (iterate-internal1 proc reversed-prefix suffix)
  (cond ((null? reversed-prefix) (proc suffix))
        ((eqv? (car reversed-prefix) #\?)
              (iterate-internal1 proc (cdr reversed-prefix) (cons #\# suffix))
              (iterate-internal1 proc (cdr reversed-prefix) (cons #\. suffix)))
        (else (iterate-internal1 proc (cdr reversed-prefix)
                                      (cons (car reversed-prefix) suffix)))))

(define (count-matches1 l ref)
  (let ((acc 0))
    (iterate-internal1
      (lambda (result) (if (equal? (count-broken result) ref)
                           (set! acc (add1 acc))))
      (reverse l) '())
    acc))

(define (prefix? small large)
  (cond ((null? small) #t)
        ((null? large) #f)
        ((eqv? (car small) (car large)) (prefix? (cdr small) (cdr large)))
        (else (and (<= (car small) (car large)) (null? (cdr small))))))

(define (count-matches l ref)
  (let ((acc 0))
    (let internal ((rev-prefix '())
                   (counts '())
                   (new-streak #t)
                   (todo l))
      (cond ((null? todo)
                (if (equal? (reverse counts) ref)
                    (set! acc (add1 acc))))
            ((not (prefix? (reverse counts) ref)))
            ((eqv? (car todo) #\.) (internal (cons #\. rev-prefix)
                                             counts
                                             #t
                                             (cdr todo)))
            ((eqv? (car todo) #\#) (internal (cons #\# rev-prefix)
                                             (if new-streak
                                                 (cons 1 counts)
                                                 (cons (add1 (car counts))
                                                       (cdr counts)))
                                             #f
                                             (cdr todo)))
            ((eqv? (car todo) #\?) (internal rev-prefix
                                             counts
                                             new-streak
                                             (cons #\. (cdr todo)))
                                   (internal rev-prefix
                                             counts
                                             new-streak
                                             (cons #\# (cdr todo))))
            (else (assert #f))))
    acc))


;(for-each (lambda (line) (write-line (conc line " -> " (count-matches (car line) (cadr line))))) data)

(write-line (conc "First puzzle:  "
  (apply + (map (lambda (line) (count-matches (car line) (cadr line))) data))))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (pow x y)
  (let loop ((n y) (acc 1))
    (if (= 0 n) acc (loop (sub1 n) (* x acc)))))

(define (count-spaced-matches memo symbols numbers)
  (cond ((null? symbols) (if (null? numbers) 1 0))
        ((eqv? (car symbols) #\#) 0)
        (else (count-matches-2 memo (cdr symbols) numbers))))

(define (count-anchored-matches memo symbols numbers)
  (assert (not (null? numbers)))
  (cond ((null? symbols) 0)
        ((eqv? (car symbols) #\.) 0)
        ((= 1 (car numbers))
            (count-spaced-matches memo (cdr symbols) (cdr numbers)))
        (else (count-anchored-matches memo
                                      (cdr symbols)
                                      (cons (sub1 (car numbers))
                                                  (cdr numbers))))))


(define (count-matches-2 memo symbols numbers)
  (if (hash-table-exists? memo (cons symbols numbers))
      (hash-table-ref memo (cons symbols numbers))
      (let ((result
        (cond ((null? numbers) (cond ((null? symbols) 1)
                                     ((eqv? (car symbols) #\#) 0)
                                     (else (count-matches-2 memo
                                                            (cdr symbols)
                                                            numbers))))
              ((null? symbols) 0)
              ((eqv? (car symbols) #\.) (count-matches-2 memo
                                                         (cdr symbols)
                                                         numbers))
              ((eqv? (car symbols) #\#) (count-anchored-matches memo
                                                                symbols
                                                                numbers))
              (else (assert (eqv? (car symbols) #\?))
                    (+ (count-spaced-matches memo symbols numbers)
                       (count-anchored-matches memo symbols numbers))))))
;       (write-line (conc "computed " symbols " " numbers " -> " result))
        (hash-table-set! memo (cons symbols numbers) result)
        result)))

(define (unfold pat sep)
  (append pat sep pat sep pat sep pat sep pat))

(define (answer-2 line)
  (let ((symbols (unfold (car line) '(#\?)))
        (numbers (unfold (cadr line) '()))
        (memo (make-hash-table)))
    (count-matches-2 memo symbols numbers)))


;(for-each
;  (lambda (line) (write-line (conc line " -> "
;                                   (count-matches (car line) (cadr line))
;                                   " -> "
;                                   (answer-2 line))))
;  data)

(write-line (conc "Second puzzle: "
  (apply + (map (lambda (line) (answer-2 line)) data))))
