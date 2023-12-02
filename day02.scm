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

(define spaces
  (one-or-more (char-set #\space)))

(define digit
  (in char-set:digit))

(define digits
  (as-string (one-or-more digit)))

(define color
  (any-of (char-seq "red")
          (char-seq "green")
          (char-seq "blue")))

(define dice-count
  (sequence* ((count digits)
              (_ (is #\space))
              (c color))
    (result (list c count))))

(define (rgb dice-count-list)
  (map (lambda (key) (string->number (car (alist-ref key dice-count-list equal? '("0")))))
      '("red" "green" "blue")))

(define hand-shown
  (sequence* ((first dice-count)
              (rest (zero-or-more (preceded-by (char-seq ", ") dice-count))))
    (result (rgb (cons first rest)))))

(define hands-shown
  (sequence* ((first hand-shown)
              (rest (zero-or-more (preceded-by (char-seq "; ") hand-shown))))
    (result (cons first rest))))

(define game-line
  (sequence* ((_ (char-seq "Game "))
              (id digits)
              (_ (char-seq ": "))
              (hands hands-shown)
              (_ (is #\newline)))
    (result (list id hands))))

(define all-data
  (one-or-more game-line))

(define (hand-valid-1? hand)
  (and (<= (car hand) 12)
       (<= (cadr hand) 13)
       (<= (caddr hand) 14)))

(define (hands-valid-1? hands)
  (or (null? hands)
      (and (hand-valid-1? (car hands))
           (hands-valid-1? (cdr hands)))))

(define (game-term-1 game)
  (if (hands-valid-1? (cadr game))
      (string->number (car game))
      0))

(define (answer-1 game-list)
  (apply + (map game-term-1 game-list)))

(define (rgb-max rgb-list cur-max)
  (if (null? rgb-list)
      cur-max
      (rgb-max (cdr rgb-list)
               (map (lambda (x) (apply max x)) (zip (car rgb-list) cur-max)))))

(define (game-power game)
  (apply * (rgb-max (cadr game) '(0 0 0))))

(define (answer-2 game-list)
  (apply + (map game-power game-list)))

(define data (parse all-data (read-string)))
(write-line (conc "First puzzle:  " (answer-1 data)))
(write-line (conc "Second puzzle: " (answer-2 data)))
