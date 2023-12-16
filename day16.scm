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

(import (chicken io) (chicken string))

(define data-list (string-split (read-string)))

(define data-height (length data-list))
(define data-width (string-length (car data-list)))

(for-each
  (lambda (line) (assert (= (string-length line) data-width)))
  data-list)

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (next-step x y dir)
  (cond
    ((eqv? dir 'right) `(,(add1 x) ,y ,dir))
    ((eqv? dir 'left)  `(,(sub1 x) ,y ,dir))
    ((eqv? dir 'down)  `(,x ,(add1 y) ,dir))
    ((eqv? dir 'up)    `(,x ,(sub1 y) ,dir))
    (else (assert #f "Invalid direction " dir))))

(define (next-steps x y dirs)
  (map (lambda (dir) (next-step x y dir)) dirs))

(define (process-step x y dir)
  (case (string-ref (list-ref data-list y) x)
    ((#\.) (next-steps x y (list dir)))
    ((#\\) (next-steps x y (cond ((eqv? dir 'right) '(down))
                                 ((eqv? dir 'left)  '(up))
                                 ((eqv? dir 'down)  '(right))
                                 ((eqv? dir 'up)    '(left))
                                 (else (assert #f)))))
    ((#\/) (next-steps x y (cond ((eqv? dir 'right) '(up))
                                 ((eqv? dir 'left)  '(down))
                                 ((eqv? dir 'down)  '(left))
                                 ((eqv? dir 'up)    '(right))
                                 (else (assert #f)))))
    ((#\|) (next-steps x y (cond ((eqv? dir 'right) '(up down))
                                 ((eqv? dir 'left)  '(up down))
                                 ((eqv? dir 'down)  '(down))
                                 ((eqv? dir 'up)    '(up))
                                 (else (assert #f)))))
    ((#\-) (next-steps x y (cond ((eqv? dir 'right) '(right))
                                 ((eqv? dir 'left)  '(left))
                                 ((eqv? dir 'down)  '(left right))
                                 ((eqv? dir 'up)    '(left right))
                                 (else (assert #f)))))
    (else (assert #f))))

(define (dir-index dir)
  (cond
    ((eqv? dir 'right) 1)
    ((eqv? dir 'left)  2)
    ((eqv? dir 'down)  3)
    ((eqv? dir 'up)    4)
    (else (assert #f "Invalid direction " dir))))

(define answer-1
  (let ((visited (make-vector (* data-width data-height 4) #f)))
    (let loop ((todo '((0 0 right))) (acc 0))
      (if (null? todo)
          acc
          (let ((x (caar todo))
                (y (cadar todo))
                (dir (caddar todo))
                (rest (cdr todo)))
            (if (and (< -1 x data-width)
                     (< -1 y data-height))
                (let* ((base-index (* 4 (+ (* data-width y) x)))
                       (index      (+ base-index (dir-index dir) -1))
                       (seen       (or (vector-ref visited base-index)
                                       (vector-ref visited (+ 1 base-index))
                                       (vector-ref visited (+ 2 base-index))
                                       (vector-ref visited (+ 3 base-index)))))
                  (if (vector-ref visited index)
                      (loop rest acc)
                      (begin
                        (vector-set! visited index #t)
                        (loop (append (process-step x y dir) rest)
                              (if seen acc (add1 acc))))))
                (loop rest acc)))))))

(write-line (conc "First puzzle:  " answer-1))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (prev-coord x y dir)
  (cond
    ((eqv? dir 'right) `(,(sub1 x) ,y))
    ((eqv? dir 'left)  `(,(add1 x) ,y))
    ((eqv? dir 'down)  `(,x ,(sub1 y)))
    ((eqv? dir 'up)    `(,x ,(add1 y)))
    (else (assert #f "Invalid direction " dir))))

(define (runner-2 start)
  (let ((visited (make-vector (* data-width data-height 4) #f)))
    (let loop ((todo (list start))
               (count 0)
               (edges (list (apply prev-coord start))))
      (if (null? todo)
          (list count edges)
          (let ((x (caar todo))
                (y (cadar todo))
                (dir (caddar todo))
                (rest (cdr todo)))
            (if (and (< -1 x data-width)
                     (< -1 y data-height))
                (let* ((base-index (* 4 (+ (* data-width y) x)))
                       (index      (+ base-index (dir-index dir) -1))
                       (seen       (or (vector-ref visited base-index)
                                       (vector-ref visited (+ 1 base-index))
                                       (vector-ref visited (+ 2 base-index))
                                       (vector-ref visited (+ 3 base-index)))))
                  (if (vector-ref visited index)
                      (loop rest count edges)
                      (begin
                        (vector-set! visited index #t)
                        (loop (append (process-step x y dir) rest)
                              (if seen count (add1 count))
                              edges))))
                (loop rest count (cons (list x y) edges))))))))

(define answer-2
  (let ((visited-top    (make-vector data-width #f))
        (visited-bottom (make-vector data-width #f))
        (visited-left   (make-vector data-height #f))
        (visited-right  (make-vector data-height #f))
        (best-score     0))
    (let* ((visited-set! (lambda (coord)
                         (cond ((= (cadr coord) data-height)
                                 (vector-set! visited-bottom (car  coord) #t))
                               ((= (cadr coord) -1)
                                 (vector-set! visited-top    (car  coord) #t))
                               ((= (car  coord) data-width)
                                 (vector-set! visited-right  (cadr coord) #t))
                               ((= (car  coord) -1)
                                 (vector-set! visited-left   (cadr coord) #t))
                               (else (assert #f "Invalid edge coord " coord)))))
           (scores-set! (lambda (coords score)
                           (for-each (lambda (coord) (visited-set! coord score))
                                     coords)))
           (run         (lambda (start)
                           (let ((result (runner-2 start)))
                             (when (> (car result) best-score)
                               (set! best-score (car result)))
                             (for-each visited-set! (cadr result))))))
      (let xloop ((x (sub1 data-width)))
        (when (not (vector-ref visited-top x))
          (run (list x 0 'down)))
        (when (not (vector-ref visited-bottom x))
          (run (list x (sub1 data-height) 'up)))
        (when (> x 0) (xloop (sub1 x))))
      (let yloop ((y (sub1 data-height)))
        (when (not (vector-ref visited-left y))
          (run (list 0 y 'right)))
        (when (not (vector-ref visited-right y))
          (run (list (sub1 data-width) y 'left)))
        (when (> y 0) (yloop (sub1 y))))
      best-score)))

(write-line (conc "Second puzzle: " answer-2))
