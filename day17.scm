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
        trace
        srfi-1
        srfi-69)

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

;; Queue management

(define (queue-line-new state score)
  (cons score state))

(define (queue-line-add queue-line state score)
  (assert (= (car queue-line) score))
  (cons score (cons state (cdr queue-line))))

(define (queue-line-rm queue-line state score)
  (assert (= (car queue-line) score))
  (let loop ((todo (cdr queue-line)) (acc '()))
    (assert (not (null? todo)))
    (if (equal? state (car todo))
        (cons score (append acc (cdr todo)))
        (loop (cdr todo) (cons (car todo) acc)))))

(define (queue-line-head-rm queue state score)
  (assert (= (caar queue) score))
  (if (= 2 (length (car queue)))
      (begin
         (assert (equal? state (cadar queue)))
         (cdr queue))
      (cons (queue-line-rm (car queue) state score)
            (cdr queue))))

(define (queue-add queue state score)
  (let loop ((todo queue) (smaller '()))
    (cond ((or (null? todo) (> (caar todo) score))
              (append (reverse smaller) `((,score ,state)) todo))
          ((= (caar todo) score)
              (append (reverse smaller)
                      `((,score ,state . ,(cdar todo)))
                      (cdr todo)))
          (else (loop (cdr todo) (cons (car todo) smaller))))))

(define (queue-update queue state old-score new-score)
  (assert (< new-score old-score))
  (let loop ((todo queue) (smaller '()) (seen #f))
    (assert (not (null? todo)))
    (cond ((= (caar todo) new-score)
             (loop (cdr todo)
                   (cons (queue-line-add (car todo) state new-score)
                         smaller)
                   #t))
          ((= (caar todo) old-score)
             (append (reverse smaller)
                     (if seen '() (list (queue-line-new state new-score)))
                     (queue-line-head-rm todo state old-score)))
          (else
             (assert (< (caar todo) old-score))
             (loop (cdr todo)
                   (cons (car todo) smaller)
                   smaller)))))

(define (make-queue scores states)
  (let loop ((todo   states)
             (result '()))
    (if (null? todo)
        result
        (loop (cdr todo)
              (queue-add result
                         (car todo)
                         (hash-table-ref scores (car todo)))))))

(define (queue-pop queue)
  (let* ((first-line (car   queue))
         (tail       (cdr   queue))
         (min-score  (car   first-line))
         (result     (cadr  first-line))
         (rest       (cddr  first-line)))
    (if (null? rest)
        (cons result tail)
        (cons result (cons (cons min-score rest) tail)))))

;; Good old Djikstra

(define answer-1
  (let ((scores  (make-hash-table))
        (sources (make-hash-table))
        (visited (make-hash-table))
        (start   '((1 0 1 right) (0 1 1 down)))
        (total   (* data-width data-height 12)))
    (for-each
      (lambda (state)
        (hash-table-set! visited state #f)
        (hash-table-set! scores  state (state->score state)))
      start)
    (let loop ((unvisited (make-queue scores start)) (result '()) (count 1))
      (if (or (null? unvisited) (= (length result) 6))
          (apply min result)
          (let* ((reordered (queue-pop unvisited))
                 (state     (car reordered))
                 (score     (hash-table-ref scores state))
                 (queue     (cdr reordered))
                 (next      (next-states-1 state))
                 (new-next  (filter
                              (lambda (s) (not (hash-table-exists? visited s)))
                              next)))
            (assert (not (hash-table-ref visited state)))
;(when (final-1? state)
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
                (let ((new-score (+ score (state->score s))))
                  (hash-table-set! visited s #f)
                  (hash-table-set! scores  s new-score)
                  (hash-table-set! sources s state)
                  (set! queue (queue-add queue s new-score))))
              new-next)
            (for-each
              (lambda (s)
                (let ((new-score (+ score (state->score s)))
                      (old-score (hash-table-ref scores s)))
                  (when (< new-score old-score)
                    (assert (not (hash-table-ref visited s)))
                    (set! queue (queue-update queue s old-score new-score))
                    (hash-table-set! scores s new-score)
                    (hash-table-set! sources s state))))
              next)
            (hash-table-set! visited state #t)
            (loop queue
                  (if (final-1? state)
                      (begin
                        (when verbose
                          (write-line (conc "Final state " state " " score)))
                        (cons score result))
                      result)
                   (add1 count)))))))

(write-line (conc "First puzzle:  " answer-1))

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

(define answer-2
  (let ((scores  (make-hash-table))
        (sources (make-hash-table))
        (visited (make-hash-table))
        (start   '((1 0 1 right) (0 1 1 down)))
        (total   (* data-width data-height 10 4)))
    (for-each
      (lambda (state)
        (hash-table-set! visited state #f)
        (hash-table-set! scores  state (state->score state)))
      start)
    (let loop ((unvisited (make-queue scores start)) (result '()) (count 1))
      (if (or (null? unvisited) (= (length result) 14))
          (apply min result)
          (let* ((reordered (queue-pop unvisited))
                 (state     (car reordered))
                 (score     (hash-table-ref scores state))
                 (queue     (cdr reordered))
                 (next      (next-states-2 state))
                 (new-next  (filter
                              (lambda (s) (not (hash-table-exists? visited s)))
                              next)))
            (assert (not (hash-table-ref visited state)))
;(when (final-2? state)
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
                (let ((new-score (+ score (state->score s))))
                  (hash-table-set! visited s #f)
                  (hash-table-set! scores  s new-score)
                  (hash-table-set! sources s state)
                  (set! queue (queue-add queue s new-score))))
              new-next)
            (for-each
              (lambda (s)
                (let ((new-score (+ score (state->score s)))
                      (old-score (hash-table-ref scores s)))
                  (when (< new-score old-score)
                    (assert (not (hash-table-ref visited s)))
                    (set! queue (queue-update queue s old-score new-score))
                    (hash-table-set! scores s new-score)
                    (hash-table-set! sources s state))))
              next)
            (hash-table-set! visited state #t)
            (loop queue
                  (if (final-2? state)
                      (begin
                        (when verbose
                          (write-line (conc "Final state " state " " score)))
                        (cons score result))
                      result)
                   (add1 count)))))))

(write-line (conc "Second puzzle: " answer-2))

