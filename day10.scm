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
  (in #\| #\- #\L #\J #\7 #\F #\. #\S))

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

(define (up xy)     (cons       (car xy)  (sub1 (cdr xy))))
(define (down xy)   (cons       (car xy)  (add1 (cdr xy))))
(define (left xy)   (cons (sub1 (car xy))       (cdr xy)))
(define (right xy)  (cons (add1 (car xy))       (cdr xy)))

(define (valid? m xy)
  (and (< -1 (car xy) (length (car m)))
       (< -1 (cdr xy) (length m))))

(define start-xy
  (let y-loop ((y 0) (lines data))
    (if (null? lines)
        (cons -1 -1)
        (let x-loop ((x 0) (line (car lines)))
           (cond ((null? line) (y-loop (add1 y) (cdr lines)))
                 ((eqv? #\S (car line)) (cons x y))
                 (else (x-loop (add1 x) (cdr line))))))))

(define (read-cell m xy)
  (assert (valid? m xy))
  (list-ref (list-ref m (cdr xy)) (car xy)))

(define (neighbors m xy)
  (case (read-cell m xy)
    ((#\|) (list (up xy) (down xy)))
    ((#\-) (list (left xy) (right xy)))
    ((#\L) (list (up xy) (right xy)))
    ((#\J) (list (up xy) (left xy)))
    ((#\7) (list (down xy) (left xy)))
    ((#\F) (list (down xy) (right xy)))
    ((#\.) '())
    ((#\S) '())
    (else (assert #f "Invalid cell data at " xy))))

(define (connected? m xy1 xy2)
  (any (lambda (xy) (equal? xy xy2)) (neighbors m xy1)))

(define data-width (length (car data)))
(define start-tile 0)
(define dist-from-start
  (let* ((result    (make-vector (* data-width (length data)) -1))
         (get-index (lambda (xy) (+ (car xy) (* (cdr xy) data-width))))
         (get-dist  (lambda (xy) (vector-ref result (get-index xy))))
         (set-dist! (lambda (xy d) (assert (= (get-dist xy) -1))
                                   (vector-set! result (get-index xy) d)))
         (start-neighbors (filter (lambda (xy) (and (valid? data xy)
                                               (connected? data xy start-xy)))
                             (list (up start-xy) (down start-xy)
                                   (left start-xy) (right start-xy)))))
    (set-dist! start-xy 0)
    (assert (= 2 (length start-neighbors)))
    (set! start-tile
      (cond ((= (caar start-neighbors) (caadr start-neighbors)) #\|)
            ((= (cdar start-neighbors) (cdadr start-neighbors)) #\-)
            ((and (or (= (caar  start-neighbors) (sub1 (car start-xy)))
                      (= (caadr start-neighbors) (sub1 (car start-xy))))
                  (or (= (cdar  start-neighbors) (sub1 (cdr start-xy)))
                      (= (cdadr start-neighbors) (sub1 (cdr start-xy))))) #\J)
            ((and (or (= (caar  start-neighbors) (sub1 (car start-xy)))
                      (= (caadr start-neighbors) (sub1 (car start-xy))))
                  (or (= (cdar  start-neighbors) (add1 (cdr start-xy)))
                      (= (cdadr start-neighbors) (add1 (cdr start-xy))))) #\7)
            ((and (or (= (caar  start-neighbors) (add1 (car start-xy)))
                      (= (caadr start-neighbors) (add1 (car start-xy))))
                  (or (= (cdar  start-neighbors) (sub1 (cdr start-xy)))
                      (= (cdadr start-neighbors) (sub1 (cdr start-xy))))) #\L)
            ((and (or (= (caar  start-neighbors) (add1 (car start-xy)))
                      (= (caadr start-neighbors) (add1 (car start-xy))))
                  (or (= (cdar  start-neighbors) (add1 (cdr start-xy)))
                      (= (cdadr start-neighbors) (add1 (cdr start-xy))))) #\F)
            (else (assert #f))))
    (let loop ((todo start-neighbors)
               (steps 1))
      (if (apply equal? todo)
          (begin
            (set-dist! (car todo) steps)
            (write-line (conc "First puzzle:  " steps)))
          (begin
;           (write-line (conc "Step " steps ": " todo))
            (for-each (lambda (xy) 
                                   (set-dist! xy steps))
                      todo)
            (loop (filter (lambda (xy) (= (get-dist xy) -1))
                          (apply append (map (lambda (xy) (neighbors data xy))
                                             todo)))
                  (add1 steps)))))
    result))

(write-line (conc "Start tile: " start-tile))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define data-height (length data))

(define (vec-ref-xy* vec x y)
  (vector-ref vec (+ x (* y data-width))))

(define (vec-ref-xy vec x y)
  (let ((result (vec-ref-xy* vec x y)))
;   (write-line (conc "vec-ref-xy " x " " y " -> " result))
    result))

(define (tile-at x y)
  (if (equal? start-xy (cons x y))
      start-tile
      (read-cell data (cons x y))))

(define answer-2
  (let loop ((x 0) (y 0) (up-before 0) (down-before 0) (acc 0))
    (cond ((>= y data-height) acc)
          ((>= x data-width) (loop 0 (add1 y) 0 0 acc))
          ((>= (vec-ref-xy dist-from-start x y) 0)
               (case (tile-at x y)
                  ((#\|)
                     (loop (add1 x) y (add1 up-before) (add1 down-before) acc))
                  ((#\-)
                     (loop (add1 x) y up-before down-before acc))
                  ((#\L #\J)
                     (loop (add1 x) y (add1 up-before) down-before acc))
                  ((#\F #\7)
                     (loop (add1 x) y up-before (add1 down-before) acc))
                  (else (assert #f))))
          ((= 1 (remainder up-before 2) (remainder down-before 2))
                     (loop (add1 x) y up-before down-before (add1 acc)))
          ((= 0 (remainder up-before 2) (remainder down-before 2))
                     (loop (add1 x) y up-before down-before acc))
          (else (assert #f)))))

(write-line (conc "Second puzzle: " answer-2))
