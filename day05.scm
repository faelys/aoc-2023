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

(define word
  (as-string (one-or-more (in char-set:letter))))

(define range
  (sequence* ((data (repeated (preceded-by spaces digits) 3))
              (_    (is #\newline)))
    (result data)))

(define number-map
  (sequence* ((_ (zero-or-more (is #\newline)))
              (from-type word)
              (_ (char-seq "-to-"))
              (to-type word)
              (_ (char-seq " map:\n"))
              (ranges (zero-or-more range)))
    (result (cons (list from-type to-type) ranges))))

(define seed-list
  (preceded-by (char-seq "seeds:")
               (zero-or-more (preceded-by spaces digits))))

(define all-data
  (sequence* ((seeds seed-list)
              (maps (one-or-more number-map)))
    (result (cons seeds maps))))

(define data (parse all-data (read-string)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (apply-map n ranges)
  (if (null? ranges)
      n
      (let ((cur-range (car ranges))
            (rest      (cdr ranges)))
        (let ((dest-start (car   cur-range))
              (src-start  (cadr  cur-range))
              (size       (caddr cur-range)))
          (if (<= src-start n (+ src-start size -1))
              (+ dest-start (- n src-start))
              (apply-map n rest))))))

(define (multi-apply-map n-list num-map)
  (map (lambda (n) (apply-map n (cdr num-map))) n-list))

(define (answer-1 nums maps)
  (if (null? maps)
      nums
      (answer-1 (multi-apply-map nums (car maps)) (cdr maps))))

(write-line (conc "First puzzle:  "
                  (apply min (answer-1 (car data) (cdr data)))))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (bounds->range range-first range-last)
  (list range-first (- range-last range-first -1)))

(define (transform-range range transform)
; (write-line (conc "transform-range " range " " transform))
  (let ((range-first  (car   range))
        (range-length (cadr  range))
        (dest-first   (car   transform))
        (src-first    (cadr  transform))
        (size         (caddr transform)))
    (let ((range-last (+ range-first range-length -1))
          (src-last (+ src-first size -1)))
;     (write-line (conc "range: " range-first " .. " range-last))
;     (write-line (conc "src: " src-first " .. " src-last))
      (list (if (and (<= range-first src-last) (>= range-last src-first))
                (list (list
                  (+ (max range-first src-first) (- dest-first src-first))
                  (- (min range-last src-last) (max range-first src-first) -1)))
                '())
            (filter (lambda (l) (not (null? l)))
               (list (if (< range-first src-first)
                         (bounds->range range-first
                                        (min (sub1 src-first) range-last))
                         '())
                     (if (> range-last src-last)
                         (bounds->range (max (add1 src-last) range-first)
                                        range-last)
                         '())))))))

(define (transform-ranges todo transformed unchanged transform)
; (write-line (conc "transform-ranges " todo " /  " transformed " / " unchanged " / " transform))
  (if (null? todo)
      (list transformed unchanged)
      (let ((tmp (transform-range (car todo) transform)))
         (transform-ranges (cdr todo)
                           (append (car tmp) transformed)
                           (append (cadr tmp) unchanged)
                           transform))))

(define (map-ranges transformed unchanged transform-list)
; (write-line (conc "map-ranges " transformed " / " unchanged " / " transform-list))
  (if (null? transform-list)
      (append unchanged transformed)
      (let ((tmp (transform-ranges unchanged '() '() (car transform-list))))
         (map-ranges (append (car tmp) transformed)
                     (cadr tmp)
                     (cdr transform-list)))))

(define (seed-ranges cur input)
  (if (null? input)
      cur
      (seed-ranges (cons (list (car input) (cadr input)) cur)
                   (cddr input))))

(define (ranges-total-size ranges)
  (apply + (map cadr ranges)))

(define (multi-map-ranges ranges maps)
; (write-line (conc "multi-map-ranges total size: " (ranges-total-size ranges)))
  (if (null? maps)
      ranges
      (multi-map-ranges (map-ranges '() ranges (cdar maps)) (cdr maps))))

(define result-2
  (multi-map-ranges (seed-ranges '() (car data))
                    (cdr data)))

(write-line (conc "Second puzzle: " (apply min (map car result-2))))
