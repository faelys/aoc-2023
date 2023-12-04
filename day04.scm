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
        srfi-14)

;;;;;;;;;;;;;;;;;
;; Input parsing

(define spaces
  (one-or-more (is #\space)))

(define digit
  (in char-set:digit))

(define digits
  (as-string (one-or-more digit)))

(define number-list
  (sequence* ((first digits)
              (rest (zero-or-more (preceded-by spaces digits))))
    (result (map string->number (cons first rest)))))

(define card-line
  (sequence* ((_ (sequence (char-seq "Card") spaces))
              (id digits)
              (_ (sequence (is #\:) spaces))
              (winning number-list)
              (_ (sequence spaces (is #\|) spaces))
              (owned number-list)
              (_ (is #\newline)))
     (result (list (string->number id) winning owned))))

(define all-data
  (one-or-more card-line))

(define data (parse all-data (read-string)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (is-in? n l)
  (cond ((null? l) #f)
        ((= n (car l)) #t)
        (else (is-in? n (cdr l)))))

(define (count-score winning todo acc)
  (if (null? todo)
      acc
      (count-score winning
                   (cdr todo)
                   (cond ((not (is-in? (car todo) winning)) acc)
                         ((= acc 0) 1)
                         (else (* acc 2))))))

(define (card-score card)
  (let ((winning (cadr card))
        (owned (caddr card)))
    (count-score winning owned 0)))

(define (answer-1 todo acc)
  (if (null? todo) acc (answer-1 (cdr todo) (+ acc (card-score (car todo))))))

(write-line (conc "First puzzle:  " (answer-1 data 0)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (count-matches winning todo acc)
  (if (null? todo)
      acc
      (count-matches winning
                     (cdr todo)
                     (if (is-in? (car todo) winning) (add1 acc) acc))))

(define (add-copies count-vector id m n)
  (if (= 0 n) count-vector
     (begin (vector-set! count-vector id (+ (vector-ref count-vector id) m))
            (add-copies count-vector (add1 id) m (sub1 n)))))

(define (process-card count-vector card)
  (let ((id (car card))
        (winning (cadr card))
        (owned (caddr card)))
    (add-copies count-vector
                (add1 id)
                (vector-ref count-vector id)
                (count-matches winning owned 0))))

(define (process-cards count-vector todo)
  (if (null? todo)
      count-vector
      (process-cards (process-card count-vector (car todo)) (cdr todo))))

(define inst-count
  (process-cards (make-vector (add1 (length data)) 1) data))
(vector-set! inst-count 0 0)

(write-line (conc "Second puzzle: " (apply + (vector->list inst-count))))
