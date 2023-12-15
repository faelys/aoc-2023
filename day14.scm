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

(define symbol
  (in #\. #\# #\O))

(define line
  (sequence* ((symbols (zero-or-more symbol))
              (_ (is #\newline)))
    (result symbols)))

(define all-data
  (zero-or-more line))

(define data (parse all-data (read-string)))

(define (draw-area prefix m)
  (unless (null? m)
    (write-line (conc prefix (list->string (car m))))
    (draw-area prefix (cdr m))))

;(write-line "Input:")
;(draw-area "  " data)

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define width (length (car data)))
(define height (length data))
(define (index-xy x y) (+ x (* width y)))
(define (get-xy vec x y) (vector-ref vec (index-xy x y)))
(define (set-xy! vec x y v) (vector-set! vec (index-xy x y) v))

(define data-vec
  (let ((result (make-vector (* width height))))
    (let loop ((x 0) (y 0) (line (car data)) (rest (cdr data)))
      (if (null? line)
          (if (null? rest)
              result
              (loop 0 (add1 y) (car rest) (cdr rest)))
          (begin
            (set-xy! result x y (car line))
            (loop (add1 x) y (cdr line) rest))))))

(define (iter-vec proc)
  (let loop ((x 0) (y 0))
    (cond ((>= y height) proc)
          ((>= x width) (loop 0 (add1 y)))
          (else (proc x y)
                (loop (add1 x) y)))))

(define (bump-north! vec x y)
  (when (and (> y 0)
             (eqv? (get-xy vec x y) #\O)
             (eqv? (get-xy vec x (sub1 y)) #\.))
    (set-xy! vec x y #\.)
    (set-xy! vec x (sub1 y) #\O)
    (bump-north! vec x (sub1 y))))

(define (tilt-north! vec)
  (iter-vec (lambda (x y) (bump-north! vec x y)))
  vec)

(define (answer-1 vec)
  (let ((acc 0))
    (iter-vec
      (lambda (x y)
        (when (eqv? (get-xy vec x y) #\O)
          (set! acc (+ acc (- height y))))))
    acc))

(tilt-north! data-vec)

(write-line (conc "First puzzle:  " (answer-1 data-vec)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (bump! vec x y dx dy)
  (let ((tx (+ x dx)) (ty (+ y dy)))
    (when (and (< -1 ty height)
               (< -1 tx width)
               (eqv? (get-xy vec x y) #\O)
               (eqv? (get-xy vec tx ty) #\.))
      (set-xy! vec x y #\.)
      (set-xy! vec tx ty #\O)
      (bump! vec tx ty dx dy))))

(define (rev-iter-vec proc)
  (let loop ((x (- width 1)) (y (- height 1)))
    (cond ((< y 0) proc)
          ((< x 0) (loop (- width 1) (sub1 y)))
          (else (proc x y)
                (loop (sub1 x) y)))))

(define (tilt! vec dx dy)
  (if (or (> dx 0) (> dy 0))
      (rev-iter-vec (lambda (x y) (bump! vec x y dx dy)))
      (iter-vec (lambda (x y) (bump! vec x y dx dy)))))

(define (finish-cycle! vec)
  (tilt! vec -1 0)
  (tilt! vec 0 1)
  (tilt! vec 1 0))

(define (cycle! vec)
  (tilt! vec 0 -1)
  (finish-cycle! vec))

(finish-cycle! data-vec)

(define (draw-area-vec vec)
  (let outer-loop ((index 0))
    (unless (>= index (* width height))
      (let inner-loop ((x (- width 1)) (acc '()))
        (if (< x 0)
            (write-line (list->string acc))
            (inner-loop (sub1 x) (cons (vector-ref vec (+ index x)) acc))))
      (outer-loop (+ index width)))))

;(draw-area-vec data-vec)
;(write-line "---")
;(tilt! data-vec -1 0)
;(draw-area-vec data-vec)
;(write-line "---")
;(tilt! data-vec 0 1)
;(draw-area-vec data-vec)

(define (save!-and-equal? source target)
  (let loop ((index (- (* width height) 1)) (result #t))
    (if (< index 0)
        result
        (let* ((val (vector-ref source index))
               (next-result (and result (eqv? (vector-ref target index) val))))
          (vector-set! target index val)
          (loop (sub1 index) next-result)))))

(define (vector-sig vec)
  (let loop ((index 0) (acc 0))
    (if (>= index (vector-length vec))
        acc
        (loop (add1 index)
              (+ (case (vector-ref vec index)
                       ((#\.) 0)
                       ((#\#) 1)
                       ((#\O) 2)
                       (else (assert #f)))
                 (* 3 acc))))))

(define memo (make-hash-table))
(define memo-period 0)
(define memo-length 0)

(define start-sig (vector-sig data-vec))

(let fill-memo ((n 1) (sig start-sig))
  (if (hash-table-exists? memo sig)
      (begin
        (set! memo-length n)
        (set! memo-period (- n (cadr (hash-table-ref memo sig))))
        (write-line (conc "Memo table filled in " n " cycles, "
                          " period " memo-period)))
      (begin
        (cycle! data-vec)
        (let ((new-sig (vector-sig data-vec)))
          (hash-table-set! memo sig (list new-sig n (answer-1 data-vec)))
          (fill-memo (add1 n) new-sig)))))

(define (answer-2 sig n)
  (cond ((> n (+ memo-period memo-length))
           (answer-2 sig
                     (+ memo-length
                        (remainder (- n memo-length) memo-period))))
        ((= n 1) (caddr (hash-table-ref memo sig)))
        (else (answer-2 (car (hash-table-ref memo sig)) (sub1 n)))))

(write-line (conc "Second puzzle: " (answer-2 start-sig (- 1000000000 1))))
