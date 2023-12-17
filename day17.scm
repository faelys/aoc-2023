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
        srfi-1
        srfi-69
        srfi-128
        srfi-146)

(define verbose #t)

(define data-list (string-split (read-string)))

(define data-height (length data-list))
(define data-width (string-length (car data-list)))

(for-each
  (lambda (line) (assert (= (string-length line) data-width)))
  data-list)

(define data-vec
  (list->vector
    (map (lambda (c) (- (char->integer c) 48))
         (apply append (map string->list data-list)))))
(assert (= (vector-length data-vec) (* data-width data-height)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (score x y)
  (vector-ref data-vec (+ (* data-width y) x)))

;; State transition function

(define (turn-left dir)
  (cond ((eqv? dir 'right) 'up)
        ((eqv? dir 'left)  'down)
        ((eqv? dir 'down)  'right)
        ((eqv? dir 'up)    'left)
        (else (assert #f "Invalid direction " dir))))

(define (turn-right dir)
  (cond ((eqv? dir 'right) 'down)
        ((eqv? dir 'left)  'up)
        ((eqv? dir 'down)  'left)
        ((eqv? dir 'up)    'right)
        (else (assert #f "Invalid direction " dir))))

(define (state->score state)
  (score (car state) (cadr state)))

(define (unchecked-next-state state)
  (let ((x     (car    state))
        (y     (cadr   state))
        (steps (caddr  state))
        (dir   (cadddr state)))
    (cond ((eqv? dir 'right) `(,(add1 x) ,y ,(add1 steps) ,dir))
          ((eqv? dir 'left)  `(,(sub1 x) ,y ,(add1 steps) ,dir))
          ((eqv? dir 'down)  `(,x ,(add1 y) ,(add1 steps) ,dir))
          ((eqv? dir 'up)    `(,x ,(sub1 y) ,(add1 steps) ,dir))
          (else (assert #f "Invalid direction " dir)))))

(define (state-valid-1? state)
  (let ((x     (car    state))
        (y     (cadr   state))
        (steps (caddr  state))
        (dir   (cadddr state)))
    (and (< -1 x data-width)
         (< -1 y data-height)
         (<= steps 3))))

(define (next-states-1 state)
  (let ((x     (car    state))
        (y     (cadr   state))
        (steps (caddr  state))
        (dir   (cadddr state)))
    (filter! state-valid-1?
             (map! unchecked-next-state
                   `((,x ,y ,steps, dir)
                     (,x ,y    0    ,(turn-left dir))
                     (,x ,y    0    ,(turn-right dir)))))))

(define (final-1? state)
  (and (= (car  state) (sub1 data-width))
       (= (cadr state) (sub1 data-height))))

(define (write-scores scores)
  (let loop ((suffixes '((1 left) (1 right) (1 up) (1 down)
                         (2 left) (2 right) (2 up) (2 down)
                         (3 left) (3 right) (3 up) (3 down))))
    (unless (null? suffixes)
      (write-line (conc (car suffixes) ":"))
      (let yloop ((y 0))
        (when (< y data-height)
          (write-line (apply conc
            (let xloop ((x (sub1 data-width)) (acc '()))
              (if (< x 0)
                  (cons "   " acc)
                  (xloop (sub1 x) (cons " " (cons (hash-table-ref/default scores (cons x (cons y (car suffixes))) "???") acc)))))))
          (yloop (add1 y))))
      (loop (cdr suffixes)))))

(define (state-part->number x)
  (cond ((number? x) x)
        ((eqv? x 'right) 0)
        ((eqv? x 'left)  1)
        ((eqv? x 'down)  2)
        ((eqv? x 'up)    3)
        (else (assert #f))))

(define (state-index state)
  (let ((x     (car    state))
        (y     (cadr   state))
        (steps (caddr  state))
        (dir   (cadddr state)))
    (+ x (* data-width
    (+ y (* data-height
    (+ (state-part->number dir) (* 4 steps))))))))

(define max-state-index (* data-width data-height 50))

;; Queue management

(define (recursive-less a b)
    (let ((aa (state-part->number (car a))) (bb (state-part->number (car b))))
      (or (< aa bb)
          (and (= aa bb)
               (recursive-less (cdr a) (cdr b))))))

(define (scored-state? s)
  (and (pair? s)
       (number? (car s)) ; score
       (number? (cadr s)) ; x
       (number? (caddr s)) ; y
       (number? (cadddr s)) ; steps
       (symbol? (car (cddddr s))) ; direction
       (null?   (cdr (cddddr s)))))

(define scored-state-comparator
  (make-comparator scored-state? equal? recursive-less default-hash))

(define (queue-add queue state score)
  (mapping-set! queue (cons score state) #t))

(define (queue-update queue state old-score new-score)
  (mapping-set!
    (mapping-delete! queue (cons old-score state))
    (cons new-score state)))

(define (make-queue scores states)
  (let loop ((todo   states)
             (result (mapping scored-state-comparator)))
    (if (null? todo)
        result
        (loop (cdr todo)
              (queue-add result
                         (car todo)
                         (vector-ref scores (state-index (car todo))))))))

(define (queue-pop queue)
  (let ((result (mapping-min-key queue)))
    (cons (cdr result) (mapping-delete! queue result))))

;; Good old Djikstra

(define (answer a b next-states final?)
  (let ((scores  (make-vector max-state-index 0))
        (visited (make-vector max-state-index 0))
        (start   '((1 0 1 right) (0 1 1 down)))
        (total   (* data-width data-height a)))
    (for-each
      (lambda (state)
        (vector-set! visited (state-index state) 1)
        (vector-set! scores (state-index state) (state->score state)))
      start)
    (let loop ((unvisited (make-queue scores start)) (result '()) (count 1))
      (if (or (null? unvisited) (= (length result) b))
          (apply min result)
          (let* ((reordered (queue-pop unvisited))
                 (state     (car reordered))
                 (score     (vector-ref scores (state-index state)))
                 (queue     (cdr reordered))
                 (next      (next-states state))
                 (new-next  (filter
                              (lambda (s) (= (vector-ref visited (state-index s)) 0))
                              next)))
            (assert (= (vector-ref visited (state-index state)) 1))
;(when (final? state)
;  (write-line (conc "final score " score))
;  (let local-loop ((s state))
;    (write-line (conc "  from " s " at " (hash-table-ref scores s)))
;    (when (hash-table-exists? sources s)
;      (local-loop (hash-table-ref sources s)))))
;(write-line (conc "State " state " " score))
;(write-line (conc "  next: " next))
;(write-line (conc "  new: " new-next))
(write-string (conc (quotient (* 100 count) total) "%\r"))
            (for-each
              (lambda (s)
                (let ((new-score (+ score (state->score s)))
                      (index (state-index s)))
                  (vector-set! visited index 1)
                  (vector-set! scores index new-score)
                  (set! queue (queue-add queue s new-score))))
              new-next)
            (for-each
              (lambda (s)
                (let* ((new-score (+ score (state->score s)))
                       (index (state-index s))
                       (old-score (vector-ref scores index)))
                  (when (< new-score old-score)
                    (assert (= (vector-ref visited index) 1))
                    (set! queue (queue-update queue s old-score new-score))
                    (vector-set! scores index new-score))))
              next)
            (vector-set! visited (state-index state) 2)
            (loop queue
                  (if (final? state)
                      (begin
                        (when verbose
                          (write-line "")
                          (write-line (conc "Final state " state " " score)))
                        (cons score result))
                      result)
                   (add1 count)))))))

(write-line (conc "First puzzle:  " (answer 12 6 next-states-1 final-1?)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

;; New state transitions

(define (state-valid-2? state)
  (let ((x     (car    state))
        (y     (cadr   state))
        (steps (caddr  state))
        (dir   (cadddr state)))
    (and (< -1 x data-width)
         (< -1 y data-height)
         (<= steps 10))))

(define (next-states-2 state)
  (let ((x     (car    state))
        (y     (cadr   state))
        (steps (caddr  state))
        (dir   (cadddr state)))
    (filter! state-valid-2?
             (cons (unchecked-next-state state)
                   (if (>= steps 4)
                       (map! unchecked-next-state
                         `((,x ,y    0    ,(turn-left dir))
                           (,x ,y    0    ,(turn-right dir))))
                       '())))))

(define (final-2? state)
  (and (final-1? state)
       (>= (caddr state) 4)))

(write-line (conc "Second puzzle: " (answer 40 14 next-states-2 final-2?)))
