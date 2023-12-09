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
        srfi-14)

;;;;;;;;;;;;;;;;;
;; Input parsing

(define (as-number parser)
  (bind (as-string parser)
        (lambda (s)
          (result (string->number s)))))

(define spaces
  (zero-or-more (is #\space)))

(define digit
  (in char-set:digit))

(define digits
  (as-number (one-or-more digit)))

(define num
  (sequence* ((sign (maybe (is #\-)))
              (n digits))
    (result (if sign (- n) n))))

(define number-list
  (sequence* ((data (zero-or-more (preceded-by spaces num)))
              (_ (is #\newline)))
    (result data)))

(define all-data
  (one-or-more number-list))

(define data (parse all-data (read-string)))
(write-line (conc "Input: " data))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (differentiate l)
  (let loop ((prev (car l)) (rest (cdr l)) (acc '()))
    (if (null? rest)
        (reverse acc)
        (loop (car rest) (cdr rest) (cons (- (car rest) prev) acc)))))

(define (differentiate* l)
  (let ((result (differentiate l)))
    (write-line (conc "differentiate " l " -> " result))
    result))

(define (integrate first l)
  (let loop ((prev first) (rest l) (acc (list first)))
    (if (null? rest)
        (reverse acc)
        (let ((new-term (+ (car rest) prev)))
          (loop new-term (cdr rest) (cons new-term acc))))))

(define (integrate* first l)
  (let ((result (integrate first l)))
    (write-line (conc "integrate " first " " l " -> " result))
    result))

(define (process-1 l)
  (if (apply = l)
      (cons (car l) l)
      (integrate (car l) (process-1 (differentiate l)))))

(write-line (conc "First puzzle:  "
  (apply + (map (lambda (l) (last (process-1 l))) data))))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (process-2 l)
  (if (apply = l)
      (car l)
      (- (car l) (process-2 (differentiate l)))))

(write-line (conc "Second puzzle: " (apply + (map process-2 data))))
