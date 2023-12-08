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
;; Second Puzzle (inefficient)

(define (start-node? node)
  (equal? (substring node 2 3) "A"))

(define (final-node? node)
  (equal? (substring node 2 3) "Z"))

(define (follow-directions directions node-history)
  (if (null? directions)
      (reverse node-history)
      (follow-directions (cdr directions)
                         (cons
                           (follow-link (car directions) (car node-history))
                           node-history))))

(define (process-history node-history end-steps steps last-node)
  (if (null? node-history)
      (list last-node end-steps)
      (process-history (cdr node-history)
                       (if (final-node? (car node-history))
                           (cons steps end-steps)
                           end-steps)
                       (add1 steps)
                       (car node-history))))

(define (process-node node)
  (process-history (follow-directions (car data) (list node)) '() 0 '()))

(define end-table
  (alist->hash-table
    (map (lambda (key) ;(write-line (conc key " -> " (process-node key)))
            (cons key (process-node key)))
         (hash-table-keys left-link-table))))

(define (is-in? n l)
  (cond ((null? l) #f)
        ((= n (car l)) #t)
        (else (is-in? n (cdr l)))))

(define (intersect l1 l2)
  (let loop ((todo l1) (acc '()))
    (if (null? todo)
        acc
        (loop (cdr todo)
              (if (is-in? (car todo) l2) (cons (car todo) acc) acc)))))

(define (intersect-list l)
  (let loop ((todo (cdr l)) (acc (car l)))
    (if (null? todo)
        acc
        (loop (cdr todo) (intersect (car todo) acc)))))

(define (answer-2 nodes past-cycles)
  (write-line (conc "Cycle " past-cycles ": " nodes))
  (let ((local-data (map (lambda (x) (hash-table-ref end-table x)) nodes)))
    (let ((next-nodes (map car local-data))
          (end-steps (map cadr local-data)))
      (let ((merged-end-steps (intersect-list end-steps)))
         (if (null? merged-end-steps)
             (answer-2 next-nodes (add1 past-cycles))
             (+ (* past-cycles (length (car data)))
                (apply min merged-end-steps)))))))

(define start-nodes
  (filter start-node? (hash-table-keys left-link-table)))

;(write-line (conc "Second puzzle: " (answer-2 start-nodes 0)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define node-list
  (hash-table-keys left-link-table))

(define state-list
  (let loop ((directions (car data))
             (nodes node-list)
             (acc '()))
    (cond ((null? directions) acc)
          ((null? nodes) (loop (cdr directions) node-list acc))
          (else (loop directions
                      (cdr nodes)
                      (cons (cons (car nodes) directions) acc))))))

(define (next-state state)
  (let ((node (car state))
        (cur-dir (cadr state))
        (next-dir (cddr state)))
    (cons (follow-link cur-dir node)
          (if (null? next-dir) (car data) next-dir))))

; state -> '(steps-to-cycle cycle-length cycle-start)
(define state-cycle-table (make-hash-table))

(define (state-cycle! state previous-steps)
  (if (hash-table-exists? state-cycle-table state)
      (let ((entry (hash-table-ref state-cycle-table state)))
        (cond ((pair? entry) entry)
              ((integer? entry) (hash-table-set! state-cycle-table state
                                  (list 0 (- previous-steps entry) state))
                                (state-cycle! (next-state state)
                                              (add1 previous-steps)))))
      (begin
        (hash-table-set! state-cycle-table state previous-steps)
        (let ((next-result
                 (state-cycle! (next-state state) (add1 previous-steps))))
           (if (integer? (hash-table-ref state-cycle-table state))
               (hash-table-set! state-cycle-table state
                  (cons (add1 (car next-result)) (cdr next-result))))
           (hash-table-ref state-cycle-table state)))))

; state-in-cycle -> '(cycle-length steps-to-ends-in-cycle)
; state-out-of-cycle
;    -> '(steps-to-ends-before-cycle steps-to-cycle
;         cycle-length steps-to-ends-in-cycle)
(define state-ends-table (make-hash-table))

(define (steps-to-ends-in-cycle from-state)
  (let loop ((state (next-state from-state))
             (steps 1)
             (acc (if (final-node? (car from-state)) '(0) '())))
    (if (equal? state from-state)
        acc
        (loop (next-state state)
              (add1 steps)
              (if (final-node? (car state))
                  (cons steps acc)
                  acc)))))

(define (build-entry state steps steps-to-ends-before-cycle)
  (let ((entry (hash-table-ref state-ends-table state)))
    (if (and (pair? entry) (= (length entry) 2))
        (cons steps-to-ends-before-cycle (cons steps entry))
        (build-entry (next-state state)
                     (add1 steps)
                     (if (final-node? (car state))
                         (cons steps steps-to-ends-before-cycle)
                         steps-to-ends-before-cycle)))))


(define (state-ends! state previous-steps)
  (if (hash-table-exists? state-ends-table state)
      (let ((entry (hash-table-ref state-ends-table state)))
        (cond ((pair? entry) entry)
              ((integer? entry)
                  (hash-table-set! state-ends-table state
                     (list (- previous-steps entry)
                           (steps-to-ends-in-cycle state)))
                  (state-ends! (next-state state) (add1 previous-steps))
                  (hash-table-ref state-ends-table state))))
      (begin
        (hash-table-set! state-ends-table state previous-steps)
        (let ((next-result
                (state-ends! (next-state state) (add1 previous-steps))))
          (if (integer? (hash-table-ref state-ends-table state))
              (hash-table-set! state-ends-table state
                 (build-entry state 0 '())))
          (hash-table-ref state-ends-table state)))))

(define start-node-list
  (filter start-node? node-list))

(write-line (conc "Start nodes: " start-node-list))

(define data-2
  (map (lambda (node) (state-ends! (cons node (car data)) 0)) start-node-list))

(let loop ((rest data-2))
  (unless (null? rest)
    (write-line (conc (car rest)))
    (loop (cdr rest))))

(write-line (conc "Second puzzle: " ))
