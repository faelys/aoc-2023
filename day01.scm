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

(import (chicken io) (chicken string))

(define (line-value-1 line first last)
  (cond ((null? line) (+ (* first 10) last))
        ((char-numeric? (car line))
           (let ((val (- (char->integer (car line)) 48)))
              (line-value-1 (cdr line)
                            (if (>= first 0) first val)
                            val)))
        (else (line-value-1 (cdr line) first last))))

(define (is-at? text pat pos)
  (and (>= (- (string-length text) pos) (string-length pat))
       (equal? (substring text pos (+ pos (string-length pat))) pat)))

(define (line-value-2 line pos first last)
  (if (> pos (string-length line))
      (+ (* first 10) last)
      (let ((val (cond ((is-at? line "one"   pos) 1)
                       ((is-at? line "two"   pos) 2)
                       ((is-at? line "three" pos) 3)
                       ((is-at? line "four"  pos) 4)
                       ((is-at? line "five"  pos) 5)
                       ((is-at? line "six"   pos) 6)
                       ((is-at? line "seven" pos) 7)
                       ((is-at? line "eight" pos) 8)
                       ((is-at? line "nine"  pos) 9)
                       ((is-at? line "0"     pos) 0)
                       ((is-at? line "1"     pos) 1)
                       ((is-at? line "2"     pos) 2)
                       ((is-at? line "3"     pos) 3)
                       ((is-at? line "4"     pos) 4)
                       ((is-at? line "5"     pos) 5)
                       ((is-at? line "6"     pos) 6)
                       ((is-at? line "7"     pos) 7)
                       ((is-at? line "8"     pos) 8)
                       ((is-at? line "9"     pos) 9)
                       (else -1))))
        (if (= val -1)
            (line-value-2 line (+ pos 1) first last)
            (line-value-2 line (+ pos 1) (if (>= first 0) first val) val)))))



(define (process acc1 acc2)
  (let ((line (read-line)))
    (if (eof-object? line)
        (output acc1 acc2)
        (process (+ acc1 (line-value-1 (string->list line) -1 -1))
                 (+ acc2 (line-value-2 line 0 -1 -1))))))

(define (output acc1 acc2)
  (write-line (conc "First puzzle:  " acc1))
  (write-line (conc "Second puzzle: " acc2)))

(process 0 0)
