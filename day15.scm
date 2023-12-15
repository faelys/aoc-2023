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

(define data (read-line))

(define verbose #f)

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (update-hash hash c)
  (remainder (* 17 (+ hash (char->integer c))) 256))

(define (answer-1 str)
  (let loop ((todo (string->list str))
             (hash 0)
             (acc 0))
    (cond ((null? todo) (+ acc hash))
          ((eqv? (car todo) #\,) (loop (cdr todo) 0 (+ acc hash)))
          (else (loop (cdr todo)
                      (update-hash hash (car todo))
                      acc)))))

(write-line (conc "First puzzle:  " (answer-1 data)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

(define (lens->string l)
  (conc " [" (list->string (car l)) " " (cadr l) "]"))

(define (lens-list->string l)
  (let loop ((todo l) (acc '()))
    (if (null? todo)
        (apply conc (reverse acc))
        (loop (cdr todo) (cons (lens->string (car todo)) acc)))))

(define (write-box vec)
  (let loop ((index 0))
    (unless (>= index (vector-length vec))
      (let ((element (vector-ref vec index)))
        (unless (null? element)
          (write-line (conc "  " index " -> " (lens-list->string element)))))
      (loop (add1 index)))))

(define (add-lens label focal lens-list)
  (let loop ((todo lens-list)
             (acc '()))
    (cond ((null? todo)
              (reverse (cons (list label focal) acc)))
          ((equal? (caar todo) label)
              (append (reverse acc) (list (list label focal)) (cdr todo)))
          (else
              (loop (cdr todo) (cons (car todo) acc))))))

(define (add-lens! boxes label hash focal)
  (vector-set! boxes hash (add-lens label focal (vector-ref boxes hash))))

(define (rm-lens label lens-list)
  (let loop ((todo lens-list)
             (acc '()))
    (if (null? todo)
        (reverse acc)
        (loop (cdr todo)
              (if (equal? (caar todo) label)
                  acc
                  (cons (car todo) acc))))))

(define (rm-lens! boxes label hash)
  (vector-set! boxes hash (rm-lens label (vector-ref boxes hash))))

(define (answer-2 str)
  (let ((boxes (make-vector 256 '())))
    (let loop ((todo (append (string->list str) '(#\,)))
               (label '())
               (hash 0))
      (unless (null? todo)
        (case (car todo)
          ((#\-) (assert (eqv? (cadr todo) #\,))
                 (rm-lens! boxes (reverse label) hash)
                 (when verbose
                   (write-line
                     (conc "After \"" (list->string (reverse label)) "-\":"))
                   (write-box boxes))
                 (loop (cddr todo) '() 0))
          ((#\=) (assert (eqv? (caddr todo) #\,))
                 (assert (<= 48 (char->integer (cadr todo)) 57))
                 (add-lens! boxes (reverse label) hash
                            (- (char->integer (cadr todo)) 48))
                 (when verbose
                   (write-line
                     (conc "After \""
                           (list->string (reverse (cons (cadr todo)
                                                        (cons #\= label))))
                           "\":"))
                   (write-box boxes))
                 (loop (cdddr todo) '() 0))
          (else  (assert (not (eqv? (car todo) #\,)))
                 (loop (cdr todo)
                       (cons (car todo) label)
                       (update-hash hash (car todo)))))))
    (when verbose (write-line "Computing answer-2:"))
    (let loop ((box-index 1)
               (todo (vector-ref boxes 0))
               (lens-index 1)
               (acc 0))
      (if (null? todo)
          (if (< box-index 256)
              (loop (add1 box-index)
                    (vector-ref boxes box-index)
                    1
                    acc)
              acc)
          (begin
            (when verbose
               (write-line (conc
                  (lens->string (car todo)) " -> "
                  (* box-index lens-index (cadar todo)))))
            (loop box-index
                  (cdr todo)
                  (add1 lens-index)
                  (+ acc (* box-index lens-index (cadar todo)))))))))

(write-line (conc "Second puzzle: " (answer-2 data)))
