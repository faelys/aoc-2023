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

(define verbose (< data-height 25))

(for-each
  (lambda (line) (assert (= (string-length line) data-width)))
  data-list)

(define data-vec
  (list->vector
    (apply append (map string->list data-list))))
(assert (= (vector-length data-vec) (* data-width data-height)))

(define (xy->index xy)
  (assert (and (< -1 (car xy) data-width) (< -1 (cdr xy) data-height))
          "Invalid xy " xy)
  (+ (* data-width (cdr xy)) (car xy)))

(define (get-xy xy)
  (vector-ref data-vec (xy->index xy)))

(define start-index
  (let loop ((index 0))
    (if (eqv? (vector-ref data-vec index) #\.)
        index
        (loop (add1 index)))))

(define end-index
  (let loop ((index (sub1 (vector-length data-vec))))
    (if (eqv? (vector-ref data-vec index) #\.)
        index
        (loop (sub1 index)))))

(define start-xy
  (cons (remainder start-index data-width) (quotient start-index data-width)))

(define end-xy
  (cons (remainder end-index data-width) (quotient end-index data-width)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (right xy) (cons (add1 (car xy)) (cdr xy)))
(define (left  xy) (cons (sub1 (car xy)) (cdr xy)))
(define (down  xy) (cons (car xy) (add1 (cdr xy))))
(define (up    xy) (cons (car xy) (sub1 (cdr xy))))

(define (inverse dir)
  (cond ((eqv? dir right) left)
        ((eqv? dir left)  right)
        ((eqv? dir down)  up)
        ((eqv? dir up)    down)
        (else (assert #f "Invalid direction " dir))))

(define (get-xy* xy)
  (if (and (< -1 (car xy) data-width)
           (< -1 (cdr xy) data-height))
      (get-xy xy)
      #\#))

(define (move-allowed? xy dir)
  (case (get-xy* (dir xy))
    ((#\.) #t)
    ((#\#) #f)
    ((#\<) (eqv? dir left))
    ((#\>) (eqv? dir right))
    ((#\^) (eqv? dir up))
    ((#\v) (eqv? dir down))
    (else (assser #f "Unknown character " (get-xy (dir xy)) dir xy))))

(define (next-dirs-1 xy not-dir)
  (filter
    (lambda (dir) (and (not (equal? dir not-dir))
                       (move-allowed? xy dir)))
    (list right left up down)))

(define (follow next-dirs xy dir)
  (assert (not (eqv? (get-xy xy) #\#)))
  (let loop ((cur-dir dir)
             (cur-xy (dir xy))
             (steps 1))
    (let ((dirs (next-dirs cur-xy (inverse cur-dir))))
      (if (= 1 (length dirs))
          (loop (car dirs) ((car dirs) cur-xy) (add1 steps))
          (list steps cur-xy dirs)))))

(define (data-edge-list next-dirs)
  (let loop ((todo `(,start-xy))
             (visited (make-vector (vector-length data-vec) #f))
             (acc '()))
    (cond ((null? todo) acc)
          ((vector-ref visited (xy->index (car todo)))
            (loop (cdr todo) visited acc))
          (else
            (let ((next (filter (lambda (l) (not (null? l)))
                                (map (lambda (dir) (follow next-dirs (car todo) dir))
                                     (next-dirs (car todo) #f)))))
              (vector-set! visited (xy->index (car todo)) #t)
              (loop (append (map cadr next) (cdr todo))
                    visited
                    (append (map (lambda (l) (cons (car todo) l)) next)
                            acc)))))))

;(for-each
;  (lambda (l) (write-line (conc l)))
;  data-edge-list)

(define (max-path-length edge-list)
  (let loop ((todo `((,start-xy 0)))
             (result 0))
    (cond ((null? todo) result)
          ((equal? (caar todo) end-xy)
                (loop (cdr todo) (max (cadar todo) result)))
          (else (loop (append (map (lambda (l) (list (caddr l)
                                                  (+ (cadr l) (cadar todo))))
                                   (filter (lambda (l) (equal? (car l) (caar todo)))
                                           edge-list))
                              (cdr todo))
                      result)))))

;(for-each
;  (lambda (l) (write-line (conc l)))
;  all-path-lengths)

(write-line (conc "First puzzle:  "
  (max-path-length (data-edge-list next-dirs-1))))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (contains? l v)
  (cond ((null? l) #f)
        ((equal? (car l) v) #t)
        (else (contains? (cdr l) v))))

(define (extend edge-list line)
  (let ((pos (cadr line))
        (steps (car line))
        (visited (cdr line)))
    (map (lambda (l) (cons (+ steps (cadr l))
                           (cons (caddr l) visited)))
         (filter (lambda (l) (and (equal? (car l) pos)
                                  (not (contains? visited (caddr l)))))
                 edge-list))))

(define (next-dirs-2 xy not-dir)
  (filter
    (lambda (dir) (and (not (equal? dir not-dir))
                       (not (eqv? (get-xy* (dir xy)) #\#))))
    (list right left up down)))

(define (max-path-length-2 edge-list)
  (let loop ((todo `((0 ,start-xy)))
             (result 0))
    (cond ((null? todo) result)
          ((equal? (cadar todo) end-xy)
                (loop (cdr todo) (max (caar todo) result)))
          (else 
                (loop (append (extend edge-list (car todo))
                              (cdr todo))
                      result)))))

(when verbose
  (for-each
    (lambda (l) (write-line (conc l)))
    (data-edge-list next-dirs-2)))

(write-line (conc "Second puzzle: "
  (max-path-length-2 (data-edge-list next-dirs-2))))
