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

(define state-name
  (as-string (one-or-more (in char-set:letter))))

(define category-name
  (in #\x #\m #\a #\s))

(define category-assoc
  (sequence* ((name category-name)
              (_ (is #\=))
              (value digits))
    (result (list name value))))

(define conditional-transition
  (sequence* ((category category-name)
              (operator (in #\< #\>))
              (value    digits)
              (_        (is #\:))
              (target   state-name))
    (result (list category operator value target))))

(define conditional-transitions
  (sequence* ((first conditional-transition)
              (rest (zero-or-more
                       (preceded-by (is #\,) conditional-transition))))
    (result (cons first rest))))

(define process-line
  (sequence* ((label state-name)
              (_ (is #\{))
              (transitions conditional-transitions)
              (_ (is #\,))
              (fallback state-name)
              (_ (char-seq "}\n")))
    (result (list label transitions fallback))))

(define part-line
  (sequence* ((_ (is #\{))
              (first category-assoc)
              (rest (zero-or-more (preceded-by (is #\,) category-assoc)))
              (_ (char-seq "}\n")))
    (result (cons first rest))))

(define all-data
  (sequence* ((processes (one-or-more process-line))
              (_ (is #\newline))
              (parts     (one-or-more part-line)))
    (result (list processes parts))))

(define data (parse all-data (read-string)))
(define verbose (< (length (cadr data)) 10))
(when verbose (write-line (conc "Input: " data)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define process-hash
  (let ((result (make-hash-table)))
    (let loop ((todo (car data)))
      (if (null? todo)
          result
          (begin
            (hash-table-set! result (caar todo) (cdar todo))
            (loop (cdr todo)))))))

(define (part-matches? condition part)
  (let* ((cat-name   (car   condition))
         (operator   (cadr  condition))
         (ref-value  (caddr condition))
         (part-value (cadr (assv cat-name part))))
    (case operator
      ((#\<) (< part-value ref-value))
      ((#\>) (> part-value ref-value))
      (else (assert #f "Unknown operator " operator)))))

(define (apply-process process part)
  (let loop ((conds    (car  process))
             (fallback (cadr process)))
    (cond ((null? conds) fallback)
          ((part-matches? (car conds) part) (cadddr (car conds)))
          (else (loop (cdr conds) fallback)))))

(define (process-part part)
  (let loop ((state "in"))
    (if (= 1 (string-length state))
        state
        (loop (apply-process (hash-table-ref process-hash state) part)))))

(define (part-score part)
  (if (equal? (process-part part) "R")
      0
      (apply + (map cadr part))))

(write-line (conc "First puzzle:  " (apply + (map part-score (cadr data)))))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define max-val 4000)
(define min-val    1)

(define (cut-with condition range)
  (let ((operator (cadr  condition))
        (value    (caddr condition)))
    (case operator
      ((#\<) (if (< (car range) value)
                 (cons (car range) (min (sub1 value) (cdr range)))
                 '()))
      ((#\>) (if (> (cdr range) value)
                 (cons (max (add1 value) (car range)) (cdr range))
                 '()))
      (else (assert #f "Bad condition " condition)))))

(define (cut-without condition range)
  (let ((operator (cadr  condition))
        (value    (caddr condition)))
    (case operator
      ((#\>) (if (< (car range) value)
                 (cons (car range) (min value (cdr range)))
                 '()))
      ((#\<) (if (> (cdr range) value)
                 (cons (max value (car range)) (cdr range))
                 '()))
      (else (assert #f "Bad condition " condition)))))

(define (cut-volume cut condition volume)
  (let ((x-range (car    volume))
        (m-range (cadr   volume))
        (a-range (caddr  volume))
        (s-range (cadddr volume)))
    (case (car condition)
      ((#\x) (list (cut condition x-range) m-range a-range s-range))
      ((#\m) (list x-range (cut condition m-range) a-range s-range))
      ((#\a) (list x-range m-range (cut condition a-range) s-range))
      ((#\s) (list x-range m-range a-range (cut condition s-range)))
      (else (assert #f "Bad condition " condition)))))

(define (volume-valid? volume)
  (not (or (null? (car    volume))
           (null? (cadr   volume))
           (null? (caddr  volume))
           (null? (cadddr volume)))))

(define (apply-process-2 process volume)
  (let loop ((conds    (car  process))
             (fallback (cadr process))
             (cur-vol  volume)
             (acc      '()))
    (if (or (null? conds) (not (volume-valid? cur-vol)))
        (filter! (lambda (item) (volume-valid? (cadr item)))
                 (cons (list fallback cur-vol) acc))
        (loop (cdr conds)
              fallback
              (cut-volume cut-without (car conds) cur-vol)
              (cons (list (cadddr (car conds))
                          (cut-volume cut-with (car conds) cur-vol))
                    acc)))))

(define (range-size range)
  (- (cdr range) (car range) -1))

(define (volume-size vol)
  (assert (= 4 (length vol)))
  (* (range-size (car    vol))
     (range-size (cadr   vol))
     (range-size (caddr  vol))
     (range-size (cadddr vol))))

(define (answer-2-iter state count next-states)
  (let loop ((todo (apply-process-2 (hash-table-ref process-hash (car state))
                                    (cadr state)))
             (acc count)
             (result next-states))
    (if (null? todo)
        (list result acc)
        (loop (cdr todo)
              (if (equal? (caar todo) "A")
                  (+ acc (volume-size (cadar todo)))
                  acc)
              (if (> (string-length (caar todo)) 1)
                  (cons (car todo) result)
                  result)))))

(define (answer-2 state-list acc)
  (if (null? state-list)
      acc
      (let ((iter (answer-2-iter (car state-list) acc (cdr state-list))))
        (answer-2 (car iter) (cadr iter)))))

(define full-volume (list (cons min-val max-val)
                          (cons min-val max-val)
                          (cons min-val max-val)
                          (cons min-val max-val)))

(write-line (conc "Second puzzle: "
  (answer-2 (list (list "in" full-volume)) 0)))
