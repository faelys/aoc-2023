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

(define (prefixed-list prefix)
  (sequence* ((_ (char-seq prefix))
              (data (zero-or-more (preceded-by spaces digits)))
              (_ (is #\newline)))
    (result data)))

(define all-data
  (sequence* ((times (prefixed-list "Time:"))
              (dists (prefixed-list "Distance:")))
    (result (zip times dists))))

(define data (parse all-data (read-string)))
(write-line (conc "Input: " data))

;;;;;;;;;;;;;;;;;
;; First Puzzle

; Traveled distance: (total_time - held_time) * held_time)
; Winning held_time when  -held_time² + total_time*held_time - other_dist >0
; So bounds are (total_time ± sqrt(total_time² - 4*other_dist))/2

(define (time-breadth l)
  (let ((total-time (car l))
        (other-dist (cadr l)))
    (let* ((sqrt-discr (sqrt (- (* total-time total-time) (* 4 other-dist))))
           (lower (floor (* 0.5 (- total-time sqrt-discr))))
           (upper (ceiling (* 0.5 (+ total-time sqrt-discr)))))
      (- upper lower 1))))

(write-line (conc "First puzzle:  " (apply * (map time-breadth data))))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define concat-data
  (list (string->number (apply conc (map car data)))
        (string->number (apply conc (map cadr data)))))

(write-line (conc "Second puzzle: " (time-breadth concat-data)))
