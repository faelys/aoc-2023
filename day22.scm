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
        trace
        srfi-1
        srfi-14
        srfi-69)

;;;;;;;;;;;;;;;;;
;; Input parsing

(define (as-number parser)
  (bind (as-string parser)
        (lambda (s)
          (result (string->number s)))))

(define digit
  (in char-set:digit))

(define digits
  (as-number (one-or-more digit)))

(define data-line
  (sequence* ((x1 digits)
              (_ (is #\,))
              (y1 digits)
              (_ (is #\,))
              (z1 digits)
              (_ (is #\~))
              (x2 digits)
              (_ (is #\,))
              (y2 digits)
              (_ (is #\,))
              (z2 digits)
              (_ (is #\newline)))
    (result `(,x1 ,y1 ,z1 ,x2 ,y2 ,z2))))

(define all-data
  (one-or-more data-line))

(define data
  (parse all-data (read-string)))
(define verbose (< (length data) 10))
(when verbose (write-line (conc "Input: " data)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (list-max-xyz l)
  (foldl (lambda (acc item)
           `(,(max (car   acc) (car   item) (car   (cdddr item)))
             ,(max (cadr  acc) (cadr  item) (cadr  (cdddr item)))
             ,(max (caddr acc) (caddr item) (caddr (cdddr item)))))
         '(0 0 0)
         l))

(define (for-each-xyz proc line)
  (let ((start-x (car   line))
        (start-y (cadr  line))
        (start-z (caddr line))
        (end-x   (car   (cdddr line)))
        (end-y   (cadr  (cdddr line)))
        (end-z   (caddr (cdddr line))))
    (let loop ((x start-x) (y start-y) (z start-z))
      (cond ((> z end-z))
            ((> y end-y) (loop start-x start-y (add1 z)))
            ((> x end-x) (loop start-x (add1 y) z))
            (else (proc x y z)
                  (loop (add1 x) y z))))))

(define (fold-xyz proc init line)
  (let ((start-x (car   line))
        (start-y (cadr  line))
        (start-z (caddr line))
        (end-x   (car   (cdddr line)))
        (end-y   (cadr  (cdddr line)))
        (end-z   (caddr (cdddr line))))
    (let loop ((x start-x) (y start-y) (z start-z) (val init))
      (cond ((> z end-z) val)
            ((> y end-y) (loop start-x start-y (add1 z) val))
            ((> x end-x) (loop start-x (add1 y) z val))
            (else (loop (add1 x) y z
                        (proc val x y z)))))))

(define (data->vec l)
  (let* ((max-xyz (list-max-xyz (map cdr l)))
         (x-size  (add1 (car   max-xyz)))
         (y-size  (add1 (cadr  max-xyz)))
         (z-size  (add1 (caddr max-xyz)))
         (idx     (lambda (x y z)
                    (assert (< -1 x x-size) "Out of bound x " x x-size)
                    (assert (< -1 y y-size) "Out of bound y " y y-size)
                    (assert (< -1 z z-size) "Out of bound z " z z-size)
                    (+ x (* x-size (+ y (* y-size z))))))
         (result  (make-vector (* x-size y-size z-size) 0)))
   (let loop ((todo l))
     (if (null? todo)
         (list idx result)
         (begin
           (for-each-xyz
             (lambda (x y z)
               (assert (= 0 (vector-ref result (idx x y z))))
               (vector-set! result (idx x y z) (caar todo)))
             (cdar todo))
           (loop (cdr todo)))))))

(define (lower-line line)
  `(,(car  line)
    ,(cadr line)
    ,(sub1 (caddr line))
    ,(car  (cdddr line))
    ,(cadr (cdddr line))
    ,(sub1 (caddr (cdddr line)))))

(define (flatten-line line)
  `(,(car  line)
    ,(cadr line)
    ,(min  (caddr line) (caddr (cdddr line)))
    ,(car  (cdddr line))
    ,(cadr (cdddr line))
    ,(min  (caddr line) (caddr (cdddr line)))))

(define (drop! xyz->index vec l)
  (let* ((all-are? (lambda (line num)
                        (fold-xyz
                          (lambda (val x y z)
                            (and val
                                 (= num (vector-ref vec (xyz->index x y z)))))
                          #t
                          line)))
         (update! (lambda (line from-num to-num)
                        (for-each-xyz
                          (lambda (x y z)
                            (assert
                              (= from-num (vector-ref vec (xyz->index x y z))))
                            (vector-set! vec (xyz->index x y z) to-num))
                          line)))
         (valid? (lambda (line)
                   (and (< 0 (caddr line))
                        (< 0 (caddr (cdddr line)))
                        (all-are? (flatten-line line) 0)))))
    (let loop ((todo l) (done '()) (changed? #f))
      (if (null? todo)
          (if changed?
              (drop! xyz->index vec done)
              done)
          (let* ((num      (caar todo))
                 (line     (cdar todo))
                 (new-line (lower-line (cdar todo)))
                 (change?  (valid? new-line)))
            (assert (all-are? line num))
            (when change?
              (update! line num 0)
              (update! new-line 0 num))
            (loop (cdr todo)
                  (cons (if change? (cons num new-line) (car todo)) done)
                  (or change? changed?)))))))

(define (unique! l)
  (let ((sorted (sort! l >)))
    (let loop ((todo (cdr sorted)) (acc (list (car sorted))))
      (if (null? todo)
          acc
          (loop (cdr todo)
                (if (= (car todo) (car acc))
                    acc
                    (cons (car todo) acc)))))))

(define (dep-graph xyz->index vec l)
  (let loop ((todo l) (acc '()))
    (if (null? todo)
        acc
        (loop (cdr todo)
              (cons (if (or (= 1 (caddr (cdar todo)))
                            (= 1 (cadddr (cdddar todo))))
                        `(,(caar todo) 0)
                        (cons (caar todo) (unique!
                          (fold-xyz
                            (lambda (l x y z)
                              (let ((v (vector-ref vec (xyz->index x y z))))
                                (if (> v 0) (cons v l) l)))
                            '()
                            (flatten-line (lower-line (cdar todo)))))))
                    acc)))))

(define (answer-1 numberless-l)
  (let* ((l          (let loop ((todo numberless-l) (num 1) (acc '()))
                       (if (null? todo) acc
                           (loop (cdr todo)
                                 (add1 num)
                                 (cons (cons num (car todo)) acc)))))
         (rich-vec   (data->vec l))
         (xyz->index (car rich-vec))
         (vec        (cadr rich-vec))
         (dropped    (drop! xyz->index vec l))
         (deps       (dep-graph xyz->index vec dropped))
         (rdeps      (make-vector (add1 (length l)) '())))
    (let loop ((todo deps) (result (vector-length rdeps)))
      (cond ((null? todo) result)
            ((= 2 (length (car todo)))
               (let ((prev (vector-ref rdeps (cadar todo))))
                 (vector-set! rdeps (cadar todo) (cons (caar todo) prev))
                 (loop (cdr todo)
                       (if (null? prev) (sub1 result) result))))
            (else (loop (cdr todo) result))))))

(write-line (conc "First puzzle:  " (answer-1 data)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (answer-2 numberless-l)
  (let* ((l          (let loop ((todo numberless-l) (num 1) (acc '()))
                       (if (null? todo) acc
                           (loop (cdr todo)
                                 (add1 num)
                                 (cons (cons num (car todo)) acc)))))
         (rich-vec   (data->vec l))
         (xyz->index (car rich-vec))
         (vec        (cadr rich-vec))
         (dropped    (drop! xyz->index vec l))
         (deps       (dep-graph xyz->index vec dropped))
         (dep-vec    (make-vector (add1 (length l)) '()))
         (rdeps      (let loop ((num -1)
                                (d '())
                                (todo deps)
                                (result (make-vector (add1 (length l)) '())))
                       (if (null? d)
                           (if (null? todo)
                               result
                               (begin
                                 (vector-set! dep-vec (caar todo) (cdar todo))
                                 (loop (caar todo)
                                       (cdar todo)
                                       (cdr todo)
                                       result)))
                           (let ((prev (vector-ref result (car d))))
                             (vector-set! result (car d) (cons num prev))
                             (loop num (cdr d) todo result)))))
         (marks      (make-vector (add1 (length l)) #f))
         (unmark!    (lambda (ll) (let loop ((l ll))
                                    (unless (null? l)
                                      (vector-set! marks (car l) #f)
                                      (loop (cdr l))))))
         (any-unmarked? (lambda (ll) (let loop ((l ll))
                                       (if (null? l) #f
                                           (or (not (vector-ref marks (car l)))
                                               (loop (cdr l))))))))
    (let loop ((next-num 1)
               (todo '())
               (marked '())
               (result 0))
      (cond ((null? todo)
                (if (>= next-num (vector-length rdeps))
                    result
                    (begin
                      (unmark! marked)
                      (vector-set! marks next-num #t)
                      (loop (add1 next-num)
                            (vector-ref rdeps next-num)
                            `(,next-num)
                            result))))
            ((vector-ref marks (car todo))
                (loop next-num (cdr todo) marked result))
            ((any-unmarked? (vector-ref dep-vec (car todo)))
                (loop next-num (cdr todo) (cons (car todo) marked) result))
            (else
                (vector-set! marks (car todo) #t)
                (loop next-num
                      (append (vector-ref rdeps (car todo))
                              (cdr todo))
                      (cons (car todo) marked)
                      (add1 result)))))))

(write-line (conc "Second puzzle: " (answer-2 data)))
