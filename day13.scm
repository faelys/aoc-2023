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

(define symbol
  (in #\. #\#))

(define line
  (sequence* ((data (one-or-more symbol))
              (_ (is #\newline)))
    (result data)))

(define area
  (sequence* ((lines (one-or-more line))
              (_ (zero-or-more (is #\newline))))
    (result lines)))

(define all-data
  (zero-or-more area))

(define data (parse all-data (read-string)))

(define (draw-area prefix m)
  (unless (null? m)
    (write-line (conc prefix (list->string (car m))))
    (draw-area prefix (cdr m))))

;(let ((count 0))
;  (for-each (lambda (area) (set! count (add1 count)) (draw-area (conc count ": ") area)) data))

;(write-line "Input:")
;(draw-area "  " data)

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (is-reflect-or-null? first second)
  (or (null? first)
      (null? second)
      (and (equal? (car first) (car second))
           (is-reflect-or-null? (cdr first) (cdr second)))))

(define (is-reflect? first second)
  (and (not (null? first))
       (not (null? second))
       (is-reflect-or-null? first second)))

(define (reflect-indices l)
;(let ((result
  (let loop ((reflected (list (car l)))
             (todo (cdr l))
             (acc '()))
    (if (null? todo)
        (reverse acc)
        (loop (cons (car todo) reflected)
              (cdr todo)
              (if (is-reflect-or-null? reflected todo)
                  (cons (length reflected) acc)
                  acc)))))
;)(write-line (conc "reflect-indices " l " -> " result)) result))

(define (merge-cols left right)
;(let ((result
  (let loop ((todo-1 left)
             (todo-2 right)
             (acc '()))
    (cond ((or (null? todo-1) (null? todo-2)) (reverse acc))
          ((< (car todo-1) (car todo-2)) (loop (cdr todo-1) todo-2 acc))
          ((> (car todo-1) (car todo-2)) (loop todo-1 (cdr todo-2) acc))
          (else (loop (cdr todo-1) (cdr todo-2) (cons (car todo-1) acc))))))
;)(write-line (conc "result " left " " right " -> " result)) result))

(define (reflect-cols area)
  (let loop ((todo (cdr area))
             (acc (reflect-indices (car area))))
    (if (null? todo)
        (begin (assert (<= (length acc) 1)) acc)
        (loop (cdr todo)
              (merge-cols acc (reflect-indices (car todo)))))))

(define (term-1 area)
  (let ((cols (reflect-cols area))
        (rows (reflect-indices area)))
;   (write-line (conc "cols: " cols ", rows: " rows))
    (unless (= (+ (length cols) (length rows)) 1)
       (write-line (conc "== cols: " cols ", rows: " rows))
       (draw-area " " area))
    (assert (= 1 (+ (length cols) (length rows))))
    (+ (apply + cols) (* 100 (apply + rows)))))

;(let ((count 0))
;  (for-each
;    (lambda (area) (set! count (add1 count))
;                   (write-line (conc count ": " (term-1 area))))
;    data))

(write-line (conc "First puzzle:  "
  (apply + (map (lambda (area) (term-1 area)) data))))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (reflect-cols* area)
  (let loop ((todo (cdr area))
             (acc (reflect-indices (car area))))
    (if (null? todo)
        acc
        (loop (cdr todo)
              (merge-cols acc (reflect-indices (car todo)))))))

(define (term-1* area prev-term)
;  (write-line (conc "term-1* " area))
;(let ((result
  (let ((cols (filter (lambda (i) (not (= i prev-term))) (reflect-cols* area)))
        (rows (filter (lambda (i) (not (= prev-term (* 100 i)))) (reflect-indices area))))
;(write-line (conc "  cols: " cols ", rows: " rows))
    (if (= (+ (length cols) (length rows)) 1)
        (+ (apply + cols) (* 100 (apply + rows)))
        0)))
;)(write-line (conc "  -> " result)) result))

(define (term-2 area)
  (let ((prev-term (term-1 area)))
    (let loop ((prev-lines '())
               (prev-cols '())
               (next-cols (car area))
               (next-lines (cdr area)))
      (if (null? next-cols)
          (loop (append prev-lines (list (reverse prev-cols)))
                '()
                (car next-lines)
                (cdr next-lines))
          (let ((try (term-1* (append prev-lines
                                      (list (append (reverse prev-cols)
                                                    (if (eqv? (car next-cols) #\#)
                                                        '(#\.) '(#\#))
                                                    (cdr next-cols)))
                                      next-lines)
                               prev-term)))
            (if (> try 0)
                try
                (loop prev-lines
                      (cons (car next-cols) prev-cols)
                      (cdr next-cols)
                      next-lines)))))))

;(let ((count 0))
;  (for-each
;    (lambda (area) (set! count (add1 count))
;                   (write-line (conc count ": " (term-2 area))))
;    data))

(write-line (conc "Second puzzle: "
  (apply + (map (lambda (area) (term-2 area)) data))))
