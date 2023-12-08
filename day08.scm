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
        srfi-14
        srfi-69)

;;;;;;;;;;;;;;;;;
;; Input parsing

(define (as-number parser)
  (bind (as-string parser)
        (lambda (s)
          (result (string->number s)))))

(define spaces
  (zero-or-more (is #\space)))

(define direction
  (in #\L #\R))

(define directions
  (sequence* ((data (one-or-more direction))
              (_ (one-or-more (is #\newline))))
    (result data)))

(define node
  (as-string (repeated (in char-set:letter) 3)))

(define (punctuation p)
  (sequence spaces (is p) spaces))

(define network-line
  (sequence* ((start node)
              (_ (punctuation #\=))
              (_ (punctuation #\())
              (left node)
              (_ (punctuation #\,))
              (right node)
              (_ (punctuation #\)))
              (_ (is #\newline)))
    (result (list start left right))))

(define network
  (one-or-more network-line))

(define all-data
  (sequence directions network))

(define data (parse all-data (read-string)))
(write-line (conc "Input: " data))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define left-link-table
  (alist->hash-table
    (map (lambda (x) (cons (car x) (cadr x))) (cadr data))))

(define right-link-table
  (alist->hash-table
    (map (lambda (x) (cons (car x) (caddr x))) (cadr data))))

(define (follow-link dir node)
  (cond ((eqv? dir #\L) (hash-table-ref left-link-table node))
        ((eqv? dir #\R) (hash-table-ref right-link-table node))))

(define (count-links-until directions start stop acc)
; (write-line (conc "At step " acc ": " start))
  (cond ((equal? start stop) acc)
        ((null? directions) (count-links-until (car data) start stop acc))
        (else (count-links-until (cdr directions)
                                 (follow-link (car directions) start)
                                 stop
                                 (add1 acc)))))

;(write-line (conc "First puzzle:  " (count-links-until '() "AAA" "ZZZ" 0)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (start-node? node)
  (equal? (substring node 2 3) "A"))

(define (final-node? node)
  (equal? (substring node 2 3) "Z"))

(define node-list
  (hash-table-keys left-link-table))

(define start-node-list
  (filter start-node? node-list))

(define state-list
  (let loop ((directions (car data))
             (nodes node-list)
             (acc '()))
    (cond ((null? directions) acc)
          ((null? nodes) (loop (cdr directions) node-list acc))
          (else (loop directions
                      (cdr nodes)
                      (cons (cons (car nodes) directions) acc))))))

(define final-state-list
  (filter (lambda (state) (final-node? (car state))) state-list))

(define (next-state state)
  (let ((node (car state))
        (cur-dir (cadr state))
        (next-dir (cddr state)))
    (cons (follow-link cur-dir node)
          (if (null? next-dir) (car data) next-dir))))

(define next-final-memo (make-hash-table))

; state -> '(steps next-final-state)
(define (next-final! state)
  (cond ((final-node? (car state)) (list 0 state))
        ((hash-table-exists? next-final-memo state)
          (hash-table-ref next-final-memo state))
        (else
          (let* ((next-result (next-final! (next-state state)))
                 (result (cons (add1 (car next-result)) (cdr next-result))))
;           (write-line (conc "next-final " state " -> " result))
            (hash-table-set! next-final-memo state result)
            result))))

; position: '(state steps)
(define (update-beyond position min-steps)
  (let* ((next-final (next-final! (next-state (car position))))
         (state (cadr next-final))
         (steps (+ (cadr position) 1 (car next-final)))
         (updated-position (list state steps)))
    (if (>= steps min-steps)
        updated-position
        (update-beyond updated-position min-steps))))

(define (finished? positions)
  (apply = (map cadr positions)))

(define (update-positions positions)
  (let ((min-steps (apply min (map cadr positions)))
        (max-steps (apply max (map cadr positions))))
    (write-line (conc "Updating from steps " min-steps "+" (- max-steps min-steps)))
    (map (lambda (position) (if (< (cadr position) max-steps)
                                (update-beyond position max-steps)
                                position))
         positions)))

(define (answer-2 positions)
; (write-line (conc "Positions: " positions))
  (if (finished? positions)
      (write-line (conc "Second puzzle: " (cadar positions)))
      (answer-2 (update-positions positions))))

(define start-positions
  (map (lambda (node) (list (cons node (car data)) 0)) start-node-list))

(answer-2 (map (lambda (x) (update-beyond x 0)) start-positions))
