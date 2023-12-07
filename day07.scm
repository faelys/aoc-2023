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

(define digits
  (as-number (one-or-more digit)))

(define card
  (in #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A))

(define hand
  (sequence* ((hand (repeated card 5))
              (_ spaces)
              (bid digits)
              (_ (is #\newline)))
    (result (list hand bid))))

(define all-data
  (zero-or-more hand))

(define data (parse all-data (read-string)))
;(write-line (conc "Input: " data))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (card-value c)
  (cond ((eqv? c #\2)  2)
        ((eqv? c #\3)  3)
        ((eqv? c #\4)  4)
        ((eqv? c #\5)  5)
        ((eqv? c #\6)  6)
        ((eqv? c #\7)  7)
        ((eqv? c #\8)  8)
        ((eqv? c #\9)  9)
        ((eqv? c #\T) 10)
        ((eqv? c #\J) 11)
        ((eqv? c #\Q) 12)
        ((eqv? c #\K) 13)
        ((eqv? c #\A) 14)))

(define converted-data
  (map (lambda (x) (list (map card-value (car x)) (cadr x))) data))

(define (inner-count-cards sorted-hand prev result)
  (if (null? sorted-hand)
      result
      (inner-count-cards (cdr sorted-hand)
                   (car sorted-hand)
                   (if (= (car sorted-hand) prev)
                       (cons (add1 (car result)) (cdr result))
                       (cons 1 result)))))
 (define (count-cards sorted-hand)
   (inner-count-cards sorted-hand -1 '()))

(define (list=? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
        ((or (null? list1) (null? list2)) #f)
        ((= (car list1) (car list2)) (list=? (cdr list1) (cdr list2)))
        (else #f)))

(define (hand-kind hand)
  (let ((counts (sort (count-cards (sort hand <)) <)))
    (cond ((list=? counts '(5)) 7)  ; five of a kind
          ((list=? counts '(1 4)) 6) ; four of a kind
          ((list=? counts '(2 3)) 5) ; full house
          ((list=? counts '(1 1 3)) 4) ; three of a kind
          ((list=? counts '(1 2 2)) 3) ; two pairs
          ((list=? counts '(1 1 1 2)) 2) ; one pair
          ((list=? counts '(1 1 1 1 1)) 1) ; one pair
          (else (write-line (conc "Bad hand " hand " with counts " counts))))))

(define (list-less? list1 list2)
  (cond ((and (null? list1) (null? list2)) #f)
        ((< (car list1) (car list2)) #t)
        ((> (car list1) (car list2)) #f)
        (else (list-less? (cdr list1) (cdr list2)))))

(define (hand-less? hand1 hand2)
  (let ((kind1 (hand-kind hand1))
        (kind2 (hand-kind hand2)))
    (or (< kind1 kind2)
        (and (= kind1 kind2) (list-less? hand1 hand2)))))

(define sorted-converted-data
  (sort converted-data (lambda (x y) (hand-less? (car x) (car y)))))

(define (answer-1 todo rank result)
  (if (null? todo)
      result
      (answer-1 (cdr todo)
                (add1 rank)
                (+ result (* rank (cadar todo))))))

(write-line (conc "First puzzle:  " (answer-1 sorted-converted-data 1 0)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (count-cards sorted-hand)
  (let* ((no-J-hand (filter (lambda (x) (not (= x 1))) sorted-hand))
         (no-J-counts (sort (inner-count-cards no-J-hand -1 '()) >)))
    (if (null? no-J-counts)
        '(5)
        (cons (+ (car no-J-counts) (- 5 (length no-J-hand)))
              (cdr no-J-counts)))))

(define converted-data-2
  (map (lambda (l) (cons
    (map (lambda (x) (if (= x 11) 1 x)) (car l)) (cdr l)))
    converted-data))

(define sorted-converted-data-2
  (sort converted-data-2 (lambda (x y) (hand-less? (car x) (car y)))))

(write-line (conc "Second puzzle: " (answer-1 sorted-converted-data-2 1 0)))
