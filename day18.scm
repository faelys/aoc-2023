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
        srfi-14)

(define verbose #f)

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

(define hex-digit
  (in #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))

(define digits
  (as-number (one-or-more digit)))

(define line
  (sequence* ((dir (in #\L #\R #\U #\D))
               (_ spaces)
               (steps digits)
               (_ spaces)
               (_ (char-seq "(#"))
               (color (as-string (repeated hex-digit 6)))
               (_ (char-seq ")\n")))
    (result (list dir steps color))))

(define all-data
  (one-or-more line))

(define data (parse all-data (read-string)))
(when verbose (write-line (conc "Input: " data)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (move-xy xy dir)
  (let ((x (car xy))
        (y (cdr xy)))
    (case dir
      ((#\L) (cons (sub1 x) y))
      ((#\R) (cons (add1 x) y))
      ((#\U) (cons x (sub1 y)))
      ((#\D) (cons x (add1 y)))
      (else (assert #f "Unknown direction " dir)))))

(define (move->xy-list dir steps acc)
  (if (= 0 steps)
      acc
      (move->xy-list dir (sub1 steps) (cons (move-xy (car acc) dir) acc))))

(define (moves->xy-list move-list acc)
  (if (null? move-list)
      acc
      (moves->xy-list (cdr move-list)
                      (move->xy-list (caar move-list) (cadar move-list) acc))))

(define (bounding-box xy-list)
  (let loop ((min-x (caar xy-list))
             (min-y (cdar xy-list))
             (max-x (caar xy-list))
             (max-y (cdar xy-list))
             (todo  (cdr  xy-list)))
    (if (null? todo)
        (list min-x min-y max-x max-y)
        (loop (min min-x (caar todo))
              (min min-y (cdar todo))
              (max max-x (caar todo))
              (max max-y (cdar todo))
              (cdr todo)))))

(define (char-for prev cur next)
  (let ((right (or (= (add1 (car prev)) (car cur))
                   (= (add1 (car next)) (car cur))))
        (left  (or (= (sub1 (car prev)) (car cur))
                   (= (sub1 (car next)) (car cur))))
        (down  (or (= (add1 (cdr prev)) (cdr cur))
                   (= (add1 (cdr next)) (cdr cur))))
        (up    (or (= (sub1 (cdr prev)) (cdr cur))
                   (= (sub1 (cdr next)) (cdr cur)))))
   (cond ((and right left) #\-)
         ((and right down) #\F)
         ((and right up)   #\L)
         ((and left  down) #\7)
         ((and left  up)   #\J)
         ((and down  up)   #\|)
         (else (assert #f "Invalid combination " right left down up)))))

(define (xy-list->vec xy-list box)
  (let* ((min-x  (car  box))
         (min-y  (cadr box))
         (width  (- (caddr  box) min-x -1))
         (height (- (cadddr box) min-y -1))
         (result (make-vector (* width height) #\.)))
    (assert (equal? (car xy-list) (last xy-list)))
    (let loop ((todo (cdr xy-list)) (prev (car xy-list)))
      (unless (null? todo)
        (vector-set! result
                     (+ (- (caar todo) min-x) (* (- (cdar todo) min-y) width))
                     (char-for prev
                               (car todo)
                               (if (null? (cdr todo))
                                   (cadr xy-list) (cadr todo))))
        (loop (cdr todo) (car todo))))
    result))

(define (draw-vec xy-vec box)
  (let* ((min-x  (car  box))
         (min-y  (cadr box))
         (width  (- (caddr  box) min-x -1))
         (height (- (cadddr box) min-y -1)))
    (let yloop ((y 0))
      (when (< y height)
        (write-line (apply conc (let xloop ((x (sub1 width)) (acc '()))
          (if (< x 0)
              acc
              (xloop (sub1 x)
                     (cons (vector-ref xy-vec (+ x (* y width))) acc))))))
        (yloop (add1 y))))))

(define (answer-1 move-list)
  (let* ((border-xy     (moves->xy-list move-list '((0 . 0))))
         (border-length (sub1 (length border-xy)))
         (data-box      (bounding-box border-xy))
         (data-vec      (xy-list->vec border-xy data-box))
         (min-x         (car    data-box))
         (min-y         (cadr   data-box))
         (max-x         (caddr  data-box))
         (max-y         (cadddr data-box))
         (width         (- (caddr  data-box) min-x -1))
         (height        (- (cadddr data-box) min-y -1))
         (xy-index      (lambda (x y) (+ (- x min-x) (* (- y min-y) width)))))
    (assert (equal? (car border-xy) '(0 . 0)))
    ;(draw-vec (xy-list->vec border-xy data-box) data-box)
    (let loop ((x min-x) (y min-y)
               (up-before 0) (down-before 0)
               (result 0))
      (cond ((> y max-y) ;(draw-vec data-vec data-box)
               (+ border-length result))
            ((> x max-x) (loop min-x (add1 y) 0 0 result))
            (else (let ((char (vector-ref data-vec (xy-index x y))))
               (assert (or (not (eqv? char #\.))
                           (= (remainder up-before 2)
                              (remainder down-before 2))))
               (loop (add1 x) y
                     (case char ((#\| #\J #\L) (add1 up-before))
                                (else up-before))
                     (case char ((#\| #\7 #\F) (add1 down-before))
                                (else down-before))
                     (if (and (eqv? char #\.)
                              (= 1 (remainder up-before 2)))
(begin (vector-set! data-vec (xy-index x y) #\:)
                         (add1 result)
)
                         result))))))))

(write-line (conc "First puzzle:  " (answer-1 data)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (move-xyl xy dir steps)
  (let ((x (car xy))
        (y (cdr xy)))
    (case dir
      ((#\L) (cons (- x steps) y))
      ((#\R) (cons (+ x steps) y))
      ((#\U) (cons x (- y steps)))
      ((#\D) (cons x (+ y steps)))
      (else (assert #f "Unknown direction " dir)))))

(define (vert-edges moves xy acc)
  (if (null? moves)
      acc
      (let ((next-xy (move-xyl xy (caar moves) (cadar moves))))
        (vert-edges (cdr moves)
                    next-xy
                    (if (= (car xy) (car next-xy))
                        (cons (list (car xy) (cdr xy) (cdr next-xy)) acc)
                        acc)))))

(define (uniq l)
  (let loop ((todo (cdr l)) (acc (list (car l))))
    (if (null? todo)
        acc
        (loop (cdr todo)
              (if (= (car todo) (car acc))
                  acc
                  (cons (car todo) acc))))))

(define (vert-edge-y edges)
  (uniq (sort (apply append (map cdr edges)) >)))

(define (recursive-< a b)
  (if (null? a)
      (not (null? b))
      (and (not (null? b))
           (or (< (car a) (car b))
               (and (= (car a) (car b))
                    (recursive-< (cdr a) (cdr b)))))))

(define (width-at edges y)
  (let loop ((todo edges) (state 'out) (prev-x 0) (acc 0))
    (if (null? todo)
        (begin
          (assert (eqv? state 'out) "Inconsistent final state " state)
          acc)
        (let ((min-y (apply min (cdar todo)))
              (max-y (apply max (cdar todo)))
              (cur-x (caar todo))
              (rest  (cdr todo)))
          (cond ((or (< y min-y) (> y max-y))
                    (loop rest state prev-x acc))
                ((and (< min-y y max-y) (eqv? state 'out))
                    (loop rest 'in cur-x (add1 acc)))
                ((and (< min-y y max-y) (eqv? state 'in))
                    (loop rest 'out 0 (+ (- cur-x prev-x) acc)))
                ((= y min-y)
                    (loop rest
                          (cond ((eqv? state 'out)           'edge-out-down)
                                ((eqv? state 'in)            'edge-in-down)
                                ((eqv? state 'edge-out-down) 'out)
                                ((eqv? state 'edge-in-down)  'in)
                                ((eqv? state 'edge-out-up)   'in)
                                ((eqv? state 'edge-in-up)    'out)
                                (else (assert #f)))
                          cur-x
                          (if (eqv? state 'out)
                              (add1 acc)
                              (+ (- cur-x prev-x) acc))))
                ((= y max-y)
                    (loop rest
                          (cond ((eqv? state 'out)           'edge-out-up)
                                ((eqv? state 'in)            'edge-in-up)
                                ((eqv? state 'edge-out-down) 'in)
                                ((eqv? state 'edge-in-down)  'out)
                                ((eqv? state 'edge-out-up)   'out)
                                ((eqv? state 'edge-in-up)    'in)
                                (else (assert #f)))
                          cur-x
                          (if (eqv? state 'out)
                              (add1 acc)
                              (+ (- cur-x prev-x) acc))))
                (else (assert #f (list state prev-x cur-x min-y max-y))))))))

(define (answer-1* data)
  (let* ((edges  (sort (vert-edges data '(0 . 0) '()) recursive-<))
         (y-list (vert-edge-y edges)))
    (let loop ((y (car y-list)) (todo (cdr y-list)) (acc 0))
      (if (null? todo)
          (+ acc (width-at edges y))
          (loop (car todo)
                (cdr todo)
                (+ acc
                   (width-at edges y)
                   (if (< (add1 y) (car todo))
                       (* (- (car todo) y 1)
                          (width-at edges (add1 y)))
                       0)))))))

(define (convert-2 line)
  (let* ((n      (string->number (caddr line) 16))
         (steps  (quotient n 16))
         (numdir (remainder n 16)))
    (list (case numdir ((0) #\R) ((1) #\D) ((2) #\L) ((3) #\U)
                       (else (assert #f "Bad numdir " numdir)))
          steps)))

(define (answer-2 lines)
  (answer-1* (map convert-2 lines)))

(write-line (conc "Second puzzle: " (answer-2 data)))
