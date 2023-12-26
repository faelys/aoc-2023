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

(define letters
  (as-string (one-or-more (in char-set:letter))))

(define data-line
  (sequence* ((label letters)
              (_ (char-seq ": "))
              (first letters)
              (rest (zero-or-more (preceded-by (is #\space) letters)))
              (_ (is #\newline)))
    (result `(,label ,first . ,rest))))

(define all-data
  (one-or-more data-line))

(define data
  (parse all-data (read-string)))
(define verbose (< (length data) 15))
(when verbose (write-line (conc "Input: " data)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define node-count -1)
(define node-ids
  (let loop ((result (make-hash-table))
             (todo data)
             (next-id 1))
    (cond ((null? todo) (set! node-count (sub1 next-id)) result)
          ((null? (car todo)) (loop result (cdr todo) next-id))
          ((hash-table-exists? result (caar todo))
                (loop result (cons (cdar todo) (cdr todo)) next-id))
          (else (hash-table-set! result (caar todo) next-id)
                (when verbose (write-line (conc "Using node " (caar todo) " as " next-id)))
                (loop result (cons (cdar todo) (cdr todo)) (add1 next-id))))))

(define (unfold-edge-list data-list)
  (let loop ((todo data-list) (acc '()))
    (cond ((null? todo) acc)
          ((null? (cdar todo)) (loop (cdr todo) acc))
          (else (loop (cons (cons (caar todo) (cddar todo)) (cdr todo))
                      (cons (list (caar todo) (cadar todo)) acc))))))

(define (translate-data data-list)
  (map (lambda (l) (map (lambda (n) (hash-table-ref node-ids n)) l))
       data-list))

(define (add-edge-list! edge-vec edge-list)
  (let loop ((todo edge-list))
    (if (null? todo)
        edge-vec
        (begin
          (vector-set! edge-vec
                       (caar todo)
                       (cons (cadar todo) (vector-ref edge-vec (caar todo))))
          (vector-set! edge-vec
                       (cadar todo)
                       (cons (caar todo) (vector-ref edge-vec (cadar todo))))
          (loop (cdr todo))))))

(define (connected-components mark-vec edge-vec)
  (let loop ((start 1)
             (todo '())
             (result '()))
    (cond ((null? todo)
              (cond ((>= start (vector-length mark-vec)) result)
                    ((vector-ref mark-vec start)
                          (loop (add1 start) '() result))
                    (else (loop (add1 start)
                                (list start)
                                (cons 0 result)))))
          ((vector-ref mark-vec (car todo))
              (loop start (cdr todo) result))
          (else
              (vector-set! mark-vec (car todo) #t)
              (loop start
                    (append (vector-ref edge-vec (car todo)) (cdr todo))
                    (cons (add1 (car result)) (cdr result)))))))

(define (dedup l)
  (let loop ((todo (sort! l >)) (acc '()))
    (if (null? todo)
        acc
        (loop (cdr todo)
              (if (and (not (null? acc)) (= (car acc) (car todo)))
                  acc
                  (cons (car todo) acc))))))

(define (answer-1 edge-list seed-node)
  (let ((mark-vec (make-vector (add1 node-count)))
        (edge-vec (make-vector (add1 node-count))))
    (vector-fill! mark-vec #f)
    (vector-fill! edge-vec '())
    (add-edge-list! edge-vec edge-list)
    (vector-set! mark-vec seed-node #t)
    (let loop ((starting? #t)
               (changed? #f)
               (edges-todo (vector-ref edge-vec seed-node))
               (edges-seen '())
               (result 1))
      (cond ((and starting? (null? edges-todo))
                (loop #f #f edges-seen '() result))
            (starting?
                (vector-set! mark-vec (car edges-todo) #t)
                (loop #t #f
                      (cdr edges-todo)
                      (append (vector-ref edge-vec (car edges-todo)) edges-seen)
                      (add1 result)))
            ((null? edges-todo)
                (cond ((= 3 (length edges-seen))
                          (* result (- node-count result)))
                      (changed?
                          (loop #f #f (dedup edges-seen) '() result))
                      (else
                          (loop #t #f (dedup edges-seen) '() result))))
            ((vector-ref mark-vec (car edges-todo))
                (loop #f changed? (cdr edges-todo) edges-seen result))
            ((>= (apply + (map (lambda (n) (if (vector-ref mark-vec n) 1 0))
                               (vector-ref edge-vec (car edges-todo))))
                 2)
                (vector-set! mark-vec (car edges-todo) #t)
                (loop #f
                      #t
                      (cdr edges-todo)
                      (append (vector-ref edge-vec (car edges-todo)) edges-seen)
                      (add1 result)))
            (else
               (loop #f changed? (cdr edges-todo) (cons (car edges-todo) edges-seen) result))))))

(write-line (conc "First puzzle:  "
  (answer-1 (unfold-edge-list (translate-data data))
            (hash-table-ref node-ids (cadar data)))))
