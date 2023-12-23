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
        trace
        srfi-1
        srfi-69)

(define data-list (string-split (read-string)))

(define data-height (length data-list))
(define data-width (string-length (car data-list)))

(define verbose (< data-height 21))

(for-each
  (lambda (line) (assert (= (string-length line) data-width)))
  data-list)

(define data-vec
  (list->vector
    (apply append (map string->list data-list))))
(assert (= (vector-length data-vec) (* data-width data-height)))

(define (xy->index xy)
  (assert (and (< -1 (car xy) data-width) (< -1 (cdr xy) data-height)))
  (+ (* data-width (cdr xy)) (car xy)))

(define (get-xy xy)
  (vector-ref data-vec (xy->index xy)))

(define start-index
  (let loop ((index 0))
    (if (eqv? (vector-ref data-vec index) #\S)
        index
        (loop (add1 index)))))

(define start-xy
  (cons (remainder start-index data-width) (quotient start-index data-height)))

;;;;;;;;;;;;;;;;;
;; First Puzzle


(define (xy-valid? xy)
  (and (< -1 (car xy) data-width)
       (< -1 (cdr xy) data-height)
       (not (eqv? (get-xy xy) #\#))))

(define (right xy) (cons (add1 (car xy)) (cdr xy)))
(define (left  xy) (cons (sub1 (car xy)) (cdr xy)))
(define (down  xy) (cons (car xy) (add1 (cdr xy))))
(define (up    xy) (cons (car xy) (sub1 (cdr xy))))

(define (neighbors xy)
  (filter xy-valid? (list (left xy) (right xy) (up xy) (down xy))))

(define min-steps-vec
  (let ((result (make-vector (vector-length data-vec) -1)))
    (let loop ((todo `((0 ,start-xy))))
      (if (null? todo)
          result
          (if (= -1 (vector-ref result (xy->index (cadar todo))))
              (begin
                (vector-set! result (xy->index (cadar todo)) (caar todo))
                (loop (append (cdr todo)
                              (map (lambda (xy) (list (add1 (caar todo)) xy))
                                   (neighbors (cadar todo))))))
              (loop (cdr todo)))))))

(define (answer-1 steps)
  (let loop ((index (sub1 (vector-length min-steps-vec)))
             (acc 0))
    (if (< index 0)
        acc
        (loop (sub1 index )
              (if (and (<= 0 (vector-ref min-steps-vec index) steps)
                       (= 0 (remainder (+ (vector-ref min-steps-vec index) steps) 2)))
                  (add1 acc)
                  acc)))))

(when verbose
  (let y-loop ((y 0))
    (unless (>= y data-height)
      (write-line (apply conc (let x-loop ((x (sub1 data-width)) (acc '()))
                                (if (< x 0) acc
                                    (x-loop (sub1 x)
                                            (cons " " (cons (vector-ref min-steps-vec (xy->index (cons x y))) acc)))))))
      (y-loop (add1 y)))))

(write-line (conc "First puzzle:  " (answer-1 64)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

; Use the fact that there is no rock on an edge, but assert it first
(let loop ((x (sub1 data-width)))
  (unless (< x 0)
    (assert (xy-valid? (cons x 0)))
    (assert (xy-valid? (cons x (sub1 data-height))))
    (loop (sub1 x))))
(let loop ((y (sub1 data-height)))
  (unless (< y 0)
    (assert (xy-valid? (cons 0 y)))
    (assert (xy-valid? (cons (sub1 data-width) y)))
    (loop (sub1 y))))

; Unusued primitives to explore the data
(define (steps-xy-< a b)
  (or (< (car a) (car b))
      (and (= (car a) (car b))
           (< (caadr a) (caadr b)))
      (and (= (car a) (car b))
           (= (caadr a) (caadr b))
           (< (cdadr a) (cdadr b)))))

(define (add-step-xy steps xy acc)
  (let loop ((smaller '()) (rest acc))
    (if (and (not (null? rest)) (< (caar rest) steps))
        (loop (cons (car rest) smaller) (cdr rest))
        (append smaller (list (list steps xy)) rest))))

(define (add-step-xylist steps xy-list acc)
  (if (null? xy-list)
      acc
      (add-step-xylist steps
                       (cdr xy-list)
                       (add-step-xy steps (car xy-list) acc))))

(define (last-line data-vec)
  (let loop ((x (sub1 data-width)) (acc '()))
    (if (< x 0)
        acc
        (loop (sub1 x)
              (cons (vector-ref data-vec
                                (xy->index (cons x (sub1 data-height))))
                    acc)))))

(define (propagate-down bottom-line)
  (assert (= (length bottom-line) data-width))
  (let ((steps-vec (make-vector (vector-length data-vec) -1)))
    (let loop ((todo (let init-loop ((x 0) (start-steps bottom-line) (acc '()))
                       (if (null? start-steps)
                           (sort! acc steps-xy-<)
                           (init-loop (add1 x)
                                      (cdr start-steps)
                                      (cons (list (add1 (car start-steps))
                                                  (cons x 0))
                                            acc))))))
      (unless (null? todo)
        (if (= -1 (vector-ref steps-vec (xy->index (cadar todo))))
            (begin
              (vector-set! steps-vec (xy->index (cadar todo)) (caar todo))
              (loop (add-step-xylist (add1 (caar todo))
                                     (neighbors (cadar todo))
                                     (cdr todo))))
            (begin
              (assert (<= (vector-ref steps-vec (xy->index (cadar todo)))
                      (caar todo)))
              (loop (cdr todo))))))
    (last-line steps-vec)))

;(trace propagate-down)
;(propagate-down (propagate-down (propagate-down (propagate-down (last-line min-steps-vec)))))

;(let* ((first-bottom (last-line min-steps-vec))
;       (second-bottom (propagate-down first-bottom))
;       (third-bottom (propagate-down second-bottom)))
;  (assert (equal? third-bottom
;                  (map (lambda (s) (+ s data-height)) second-bottom))))

;; Build 5x5 copies of the original grid and solve it

(define (fold-xy xy)
  (cons (remainder (car xy) data-width)
        (remainder (cdr xy) data-height)))

(define (xy->index-2 xy)
  (assert (and (< -1 (car xy) (* 5 data-width))
               (< -1 (cdr xy) (* 5 data-height)))
          "Coordinates " xy " out of bounds")
  (+ (* 5 data-width (cdr xy)) (car xy)))

(define (xy-valid-2? xy)
  (and (< -1 (car xy) (* 5 data-width))
       (< -1 (cdr xy) (* 5 data-height))
       (not (eqv? (get-xy (cons (remainder (car xy) data-width)
                                (remainder (cdr xy) data-height)))
                  #\#))))

(define (neighbors-2 xy)
  (filter xy-valid-2? (list (left xy) (right xy) (up xy) (down xy))))

(define min-steps-vec-2
  (let ((result (make-vector (* 25 (vector-length data-vec)) -1)))
    (let loop ((todo `((0 (,(+ (car start-xy) data-width data-width)
                         . ,(+ (cdr start-xy) data-height data-height))))))
      (if (null? todo)
          result
          (if (= -1 (vector-ref result (xy->index-2 (cadar todo))))
              (begin
                (vector-set! result (xy->index-2 (cadar todo)) (caar todo))
                (loop (append (cdr todo)
                              (map (lambda (xy) (list (add1 (caar todo)) xy))
                                   (neighbors-2 (cadar todo))))))
              (loop (cdr todo)))))))

;; Double check that outer grid is the same as the inner grid

(define (valid-or-negative? val-1 val-2 dist)
  (or (= -1 val-2 val-2)
      (= val-1 (+ val-2 dist))))

(define (check-dist vec xy-1 xy-2 dist)
  (if (valid-or-negative? (vector-ref vec (xy->index-2 xy-1))
                          (vector-ref vec (xy->index-2 xy-2))
                          dist)
      #t
      (begin
        (write-line (conc "Check failed between " xy-1
                          " as " (vector-ref vec (xy->index-2 xy-1))
                          " and " xy-2
                          " as " (vector-ref vec (xy->index-2 xy-2))
                          ", expected " dist))
        #f)))

(let y-loop ((y (sub1 data-height)))
  (unless (< y 0)
    (let x-loop ((x (sub1 (* 5 data-width))))
      (unless (< x 0)
        (assert (check-dist min-steps-vec-2
                            (cons x y)
                            (cons x (+ y data-height))
                            data-height))
        (assert (check-dist min-steps-vec-2
                            (cons x (- (* 5 data-height) y 1))
                            (cons x (- (* 4 data-height) y 1))
                            data-height))
        (x-loop (sub1 x))))
    (y-loop (sub1 y))))

(let x-loop ((x (sub1 data-width)))
  (unless (< x 0)
    (let y-loop ((y (sub1 (* 5 data-height))))
      (unless (< y 0)
        (assert (check-dist min-steps-vec-2
                            (cons x y)
                            (cons (+ x data-width) y)
                            data-width))
        (assert (check-dist min-steps-vec-2
                            (cons (- (* 5 data-width) x 1) y)
                            (cons (- (* 4 data-width) x 1) y)
                            data-width))
        (y-loop (sub1 y))))
    (x-loop (sub1 x))))

;; Use periodicity to fold space upon itself

(assert (= data-width data-height))
(assert (= (remainder data-width 2) 1))
(define data-radius
  (/ (sub1 data-width) 2))

(define (count-2 vec start-x start-y end-x end-y steps)
  (let loop ((x start-x) (y start-y) (acc 0))
    (cond ((>= y end-y) acc)
          ((>= x end-x) (loop start-x (add1 y) acc))
          (else
        (loop (add1 x) y
              (if (and (= (remainder (+ x y) 2) (remainder steps 2))
                       (<= 0 (vector-ref vec (xy->index-2 (cons x y))) steps))
                  (add1 acc)
                  acc))))))

(define (count-2* start-x-block start-y-block end-x-block end-y-block steps)
  (count-2 min-steps-vec-2
           (* start-x-block data-width) (* start-y-block data-height)
           (*   end-x-block data-width) (*   end-y-block data-height)
           steps))

(define (write-count-grid steps)
  (write-line (conc (count-2* 0 0 1 1 steps) "\t"
                    (count-2* 1 0 2 1 steps) "\t"
                    (count-2* 2 0 3 1 steps) "\t"
                    (count-2* 3 0 4 1 steps) "\t"
                    (count-2* 4 0 5 1 steps)))
  (write-line (conc (count-2* 0 1 1 2 steps) "\t"
                    (count-2* 1 1 2 2 steps) "\t"
                    (count-2* 2 1 3 2 steps) "\t"
                    (count-2* 3 1 4 2 steps) "\t"
                    (count-2* 4 1 5 2 steps)))
  (write-line (conc (count-2* 0 2 1 3 steps) "\t"
                    (count-2* 1 2 2 3 steps) "\t"
                    (count-2* 2 2 3 3 steps) "\t"
                    (count-2* 3 2 4 3 steps) "\t"
                    (count-2* 4 2 5 3 steps)))
  (write-line (conc (count-2* 0 3 1 4 steps) "\t"
                    (count-2* 1 3 2 4 steps) "\t"
                    (count-2* 2 3 3 4 steps) "\t"
                    (count-2* 3 3 4 4 steps) "\t"
                    (count-2* 4 3 5 4 steps)))
  (write-line (conc (count-2* 0 4 1 5 steps) "\t"
                    (count-2* 1 4 2 5 steps) "\t"
                    (count-2* 2 4 3 5 steps) "\t"
                    (count-2* 3 4 4 5 steps) "\t"
                    (count-2* 4 4 5 5 steps))))

(define (answer-2 steps)
  (let ((reduced-steps (+ data-width data-radius (remainder (- steps data-radius) data-width)))
        (expansion-count (sub1 (quotient (- steps data-radius) data-width))))
    (write-line (conc "Expansions " expansion-count ", steps " reduced-steps))
    (write-count-grid reduced-steps)
    (write-line "")
    (write-count-grid (+ data-width reduced-steps))
    (write-line "")
    (write-count-grid (+ data-width data-width reduced-steps))
    (let ((crown (+ (count-2* 1 2 2 3 reduced-steps)
                    (count-2* 2 1 3 2 reduced-steps)
                    (count-2* 2 3 3 4 reduced-steps)
                    (count-2* 3 2 4 3 reduced-steps)))
          (edge1 (+ (count-2* 1 1 2 2 reduced-steps)
                    (count-2* 1 3 2 4 reduced-steps)
                    (count-2* 3 1 4 2 reduced-steps)
                    (count-2* 3 3 4 4 reduced-steps)))
          (edge2 (+ (count-2* 1 1 2 2 (+ reduced-steps data-width))
                    (count-2* 1 3 2 4 (+ reduced-steps data-width))
                    (count-2* 3 1 4 2 (+ reduced-steps data-width))
                    (count-2* 3 3 4 4 (+ reduced-steps data-width))))
          (middle1  (count-2* 2 2 3 3 reduced-steps))
          (middle2  (count-2* 2 2 3 3 (+ reduced-steps data-width))))
       (write-line (conc "Crown: " crown ", edges " edge1 " " edge2 ", middle " middle1 " " middle2))
       (+ crown
          (* edge1 (add1 expansion-count))
          (* edge2 expansion-count)
          (* middle1 (* (add1 expansion-count) (add1 expansion-count)))
          (* middle2 (* expansion-count expansion-count))))))

;; The loop below shows it doesn't really work,
;; but it does gives the correct answer for my input  ¯\_(ツ)_/¯
(let loop ((steps '(196 327 193 324)))
  (unless (null? steps)
    (write-line (conc "Debug: " (car steps)
                      " -> " (answer-2 (car steps))
                      " vs " (count-2* 0 0 5 5 (car steps))))
    (loop (cdr steps))))

(write-line (conc "Second puzzle:  " (answer-2 26501365)))
