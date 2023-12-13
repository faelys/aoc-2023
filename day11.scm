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
  (sequence* ((data (zero-or-more symbol))
              (_ (is #\newline)))
    (result data)))

(define all-data
  (zero-or-more line))

(define data (parse all-data (read-string)))

(define (draw-map prefix m)
  (unless (null? m)
    (write-line (conc prefix (list->string (car m))))
    (draw-map prefix (cdr m))))

;(write-line "Input:")
;(draw-map "  " data)

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (find-xy m c)
  (let loop ((y 0) (lines (cdr m)) (x 0) (chars (car m)) (acc '()))
    (if (null? chars)
        (if (null? lines)
            acc
            (loop (add1 y) (cdr lines) 0 (car lines) acc))
        (loop y lines (add1 x) (cdr chars)
              (if (eqv? (car chars) c) (cons (cons x y) acc) acc)))))

(define (count-xy xy-list)
  (let loop ((result (cons (make-vector (add1 (apply max (map car xy-list))) 0)
                           (make-vector (add1 (apply max (map cdr xy-list))) 0)))
             (todo xy-list))
    (if (null? todo)
        result
        (begin
          (vector-set! (car result) (caar todo)
                       (add1 (vector-ref (car result) (caar todo))))
          (vector-set! (cdr result) (cdar todo)
                       (add1 (vector-ref (cdr result) (cdar todo))))
          (loop result (cdr todo))))))

(define (counts->expand-vec counts age)
  (let ((result (make-vector (vector-length counts))))
    (let loop ((old-x 0) (delta-x 0))
      (unless (>= old-x (vector-length result))
        (vector-set! result old-x (+ old-x (* age delta-x)))
        (loop (add1 old-x)
              (if (= (vector-ref counts old-x) 0)
                  (add1 delta-x)
                  delta-x))))
   result))

(define (expand-xy xy-list age)
   (let* ((counts (count-xy xy-list))
          (expand-x (counts->expand-vec (car counts) age))
          (expand-y (counts->expand-vec (cdr counts) age)))
     (map (lambda (xy) (cons (vector-ref expand-x (car xy))
                             (vector-ref expand-y (cdr xy))))
          xy-list)))

(define (dist xy1 xy2)
  (+ (abs (- (car xy1) (car xy2))) (abs (- (cdr xy1) (cdr xy2)))))

(define (dist-sum xy-list acc)
  (if (null? xy-list)
      acc
      (let loop ((cur (car xy-list))
                 (todo (cdr xy-list))
                 (inner-acc acc))
        (if (null? todo)
            (dist-sum (cdr xy-list) inner-acc)
            (loop cur (cdr todo) (+ inner-acc (dist cur (car todo))))))))

(define orig-xy (find-xy data #\#))

(write-line (conc "First puzzle:  " (dist-sum (expand-xy orig-xy 1) 0)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(write-line (conc "Second puzzle: "))
(write-line (conc "       10: " (dist-sum (expand-xy orig-xy 9) 0)))
(write-line (conc "      100: " (dist-sum (expand-xy orig-xy 99) 0)))
(write-line (conc "  1000000: " (dist-sum (expand-xy orig-xy 999999) 0)))
