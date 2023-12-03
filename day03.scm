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
        srfi-1)

;; input data, as a list of strings
(define grid (read-lines))

;; derived sizes
(define width (string-length (car grid)))
(define height (length grid))
(define (xy-valid? x y)
  (and (< -1 x width) (< -1 y height)))

;; grid accessors
(define (grid-char x y)
  (if (xy-valid? x y)
      (string-ref (list-ref grid y) x)
      #\nul))

;; grid iterators
(define (next-x x)
  (if (>= x width) 0 (+ x 1)))
(define (next-y y)
  (if (>= y height) 0 (+ y 1)))
(define (last? x y)
  (and (= x (- width 1)) (= y (- height 1))))

;; symbol check
;; (in my input, symbols are "#$%&*+-./=@")
(define (char-symbol? c)
  (or (<= 35 (char->integer c) 45) (eqv? c #\/) (eqv? c #\=) (eqv? c #\@)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

;; list of (start-x end-x y value) for each part number
(define number-position-list
  (let loop ((acc '()) (start-x -1) (x 0) (y 0) (n 0))
    (cond ((>= y height) acc)
          ((>= x width)
             (loop (if (>= start-x 0) (cons (list start-x x y n) acc) acc)
                   -1 0 (+ y 1) 0))
          ((char-numeric? (grid-char x y))
             (loop acc (if (>= start-x 0) start-x x) (+ x 1) y (+ (* n 10) (char->integer (grid-char x y)) -48)))
          ((>= start-x 0)
             (loop (cons (list start-x x y n) acc) -1 (+ x 1) y 0))
          (else
             (loop acc -1 (+ x 1) y 0)))))

;; check whether a given horizontal range contains a symbol
(define (contains-symbol? start-x end-x y)
  (cond ((or (< y 0) (>= y height) (>= start-x end-x)) #f)
        ((char-symbol? (grid-char start-x y)) #t)
        (else (contains-symbol? (+ start-x 1) end-x y))))

;; check whether a symbol exists near the given number position
(define (is-part-position? pos)
  (let ((start-x (car   pos))
        (end-x   (cadr  pos))
        (y       (caddr pos)))
    (or (contains-symbol? (sub1 start-x) (add1 end-x) (sub1 y))
        (contains-symbol? (sub1 start-x) (add1 end-x) (add1 y))
        (contains-symbol? (sub1 start-x)    start-x       y)
        (contains-symbol?     end-x      (add1 end-x)     y))))

;; extract part number from all numbers
(define part-position-list (filter is-part-position? number-position-list))

;; sum of all part numbers
(define answer-1
  (let loop ((position-list part-position-list) (acc 0))
    (if (null? position-list)
        acc
        (let ((rest (cdr position-list))
              (head (car position-list)))
          (loop rest (+ acc (cadddr head)))))))
(write-line (conc "First puzzle:  " answer-1))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

;; is the given number-position adjacent to the given position
(define (adjacent? pos x y)
  (let ((start-x (car   pos))
        (end-x   (cadr  pos))
        (pos-y   (caddr pos)))
    (and (<= (sub1 pos-y) y (add1 pos-y))
         (<= (sub1 start-x) x end-x))))

;; list of all part positions adjacent to the given position
(define (adjacent-pos-list x y todo found)
  (if (null? todo)
      found
      (adjacent-pos-list x y (cdr todo)
         (if (adjacent? (car todo) x y)
             (cons (car todo) found)
             found))))

;; list of all part numbers adjacent to the given position
(define (adjacent-num-list x y)
  (map cadddr (adjacent-pos-list x y part-position-list '())))

;; compute the gear ratio
(define (gear-ratio x y not-a-gear)
  (if (eqv? (grid-char x y) #\*)
      (let ((num-list (adjacent-num-list x y)))
        (if (= (length num-list) 2)
            (apply * num-list)
            not-a-gear))
      not-a-gear))

;; iterate over the whole grid
(define answer-2
  (let loop ((x 0) (y 0) (acc 0))
    (cond ((>= y height) acc)
          ((>= x width) (loop 0 (add1 y) acc))
          (else (loop (add1 x) y (+ acc (gear-ratio x y 0)))))))
(write-line (conc "Second puzzle: " answer-2))
