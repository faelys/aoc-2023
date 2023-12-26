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

(define (as-number parser)
  (bind (as-string parser)
        (lambda (s)
          (result (string->number s)))))

(define digit
  (in char-set:digit))

(define digits
  (as-number (sequence (maybe (is #\-)) (one-or-more digit))))

(define data-line
  (sequence* ((px digits)
              (_ (char-seq ", "))
              (py digits)
              (_ (char-seq ", "))
              (pz digits)
              (_ (sequence (char-seq " @") (zero-or-more (is #\space))))
              (vx digits)
              (_ (sequence (is #\,) (zero-or-more (is #\space))))
              (vy digits)
              (_ (sequence (is #\,) (zero-or-more (is #\space))))
              (vz digits)
              (_ (is #\newline)))
    (result `(,px ,py ,pz ,vx ,vy ,vz))))

(define all-data
  (one-or-more data-line))

(define data
  (parse all-data (read-string)))
(define verbose (< (length data) 10))
(when verbose (write-line (conc "Input: " data)))

;;;;;;;;;;;;;;;;;
;; First Puzzle

(define (get-px line) (car   line))
(define (get-py line) (cadr  line))
(define (get-pz line) (caddr line))
(define (get-vx line) (car   (cdddr line)))
(define (get-vy line) (cadr  (cdddr line)))
(define (get-vz line) (caddr (cdddr line)))

(define zone-min (if verbose  7 200000000000000))
(define zone-max (if verbose 27 400000000000000))

;; 2D line is (px+t·vx, py+t·vy) i.e. (y-py)vx = (x-px)vy
;; or x·vy - y·vx + py·vx - px·vy = 0
;; intersect x·vy1·vx2 + py1·vx1·vx2 - px1·vy1·vx2 = 

(define (intersect-2d line1 line2)
  (let ((px1 (get-px line1))
        (py1 (get-py line1))
        (vx1 (get-vx line1))
        (vy1 (get-vy line1))
        (px2 (get-px line2))
        (py2 (get-py line2))
        (vx2 (get-vx line2))
        (vy2 (get-vy line2)))
    (let ((d (- (* vx1 vy2) (* vx2 vy1))))
      (if (= 0 d)
          (and (= px1 px2) (= py1 py2))
          (list (/ (+ (* py1 vx1 vx2) (* -1 px1 vy1 vx2)
                      (* px2 vy2 vx1) (* -1 py2 vx2 vx1)) d)
                (/ (+ (* -1 px1 vy1 vy2) (* py1 vx1 vy2)
                      (* -1 py2 vx2 vy1) (* px2 vy2 vy1)) d))))))

(define (check-1 line1 line2)
  (let ((i (intersect-2d line1 line2)))
    (cond ((not (pair? i))
              (when verbose (write-line "Degenerate"))
              #f)
          ((> 0 (+ (* (- (car  i) (get-px line1))
                      (get-vx line1))
                   (* (- (cadr i) (get-py line1))
                      (get-vy line1))))
              (when verbose (write-line "In the past of A: "))`
              #f)
          ((> 0 (+ (* (- (car  i) (get-px line2))
                     (get-vx line2))
                  (* (- (cadr i) (get-py line2))
                     (get-vy line2))))

              (when verbose (write-line "In the past of B"))
              #f)
          ((and (<= zone-min (car  i) zone-max)
                (<= zone-min (cadr i) zone-max))
           #t)
          (else (when verbose (write-line "Not in zone")) #f))))

(define (answer-1 dataset)
  (let loop ((todo dataset) (rest (cdr dataset)) (result 0))
    (if (null? rest)
        (if (null? (cdr todo))
            result
            (loop (cdr todo) (cddr todo) result))
          (loop todo
                (cdr rest)
                (if (check-1 (car todo) (car rest))
                    (add1 result)
                    result)))))

(write-line (conc "First puzzle:  " (answer-1 data)))

;;;;;;;;;;;;;;;;;
;; Second Puzzle

;; Giving index 0 to my stone and 1 to the hailsone, there is
;; an intersection when there exists an integer t so that
;;   px0 + t·vx0 = px1 + t·vx1   and
;;   py0 + t·vy0 = py1 + t·vy1   and
;;   pz0 + t·vz0 = pz1 + t·vz1   and
;;   t >= 0
;; which means that
;;   (px1-px0) / (vx1-vx0) = (py1-py0) / (vy1-vy0) = (pz1-pz0) / (vz1-vz0) ≤ 0
;; with all these divisions having a zero remainder.
;; So first v.0 intervals 

;;   p.0 ∈ p.1 + ℕ·lcm((vx1-vx0),(vy1
;;   ≡

(define (new-min inter target astep)
  (let ((prev-min  (car   inter))
        (prev-step (cadr  inter))
        (prev-max  (caddr inter)))
    (assert (> astep 0))
    (let loop ((result prev-min))
      (cond ((= target (remainder result astep)) result)
            ((> result prev-max) #f)
            (else (loop (+ result prev-step)))))))

(define (round-up val modulus)
  (assert (and (> val 0) (> modulus 0)))
  (let ((result (* modulus (add1 (quotient (sub1 val) modulus)))))
    (assert (and (>= result val) (< val (+ result modulus))))
    result))

(define (round-down val modulus)
  (assert (and (>= val 0) (> modulus 0)))
  (let ((result (* modulus (quotient val modulus))))
    (assert (and (<= result val) (> val (- result modulus))))
    result))

(define (adjust-up val ref modulus)
  (+ ref (round-up (- val ref) modulus)))

(define (adjust-down val ref modulus)
  (+ ref (round-down (- val ref) modulus)))

(define (update-interval inter start step)
  (let* ((prev-min  (car   inter))
         (prev-step (cadr  inter))
         (prev-max  (caddr inter))
         (astep     (abs   step))
         (new-step  (if (or (= step 0) (= astep 0)) 0 (lcm astep prev-step)))
         (div       (if (or (= step 0) (= astep 0)) 0 (gcd astep prev-step))))
    (cond ((= step 0)
              (if (and (<= prev-min start prev-max)
                       (or (= 0 prev-step)
                           (= 0 (remainder (- start prev-min) prev-step))))
                  (list start 0 start)
                  #f))
          ((= prev-step 0)
             (assert (= prev-min prev-max))
             (if (and (= (remainder prev-min astep) (remainder start astep))
                      (or (= prev-min start)
                          (and (> prev-min start) (> step 0))
                          (and (< prev-min start) (< step 0))))
                 inter
                 #f))
          ((= (remainder prev-min div) (remainder start div))
             (let* ((updated-min (new-min inter
                                          (remainder start astep)
                                          astep))
                    (result
               (cond ((not updated-min) #f)
                     ((or (> updated-min prev-max)
                          (and (> step 0) (< prev-max start))
                          (and (< step 0) (< start updated-min)))
                         #f)
                     ((> step 0)
                         (list (if (<= start updated-min)
                                   updated-min
                                   (adjust-up start updated-min new-step))
                               new-step
                               (adjust-down prev-max updated-min new-step)))
                     (else
                         (list updated-min
                               new-step
                               (adjust-down (min prev-max start)
                                            updated-min new-step))))))
               (cond ((or (not result) (> (car result) (caddr result))) #f)
                     ((= (car result) (caddr result))
                         (list (car result) 0 (caddr result)))
                     (else result))))
          (else #f))))

(define (next-v prev-v)
  (if (<= prev-v 0) (- 1 prev-v) (- prev-v)))


(define (make-inter fn v)
  (let loop ((inter (list 0 1 (* 2 zone-max)))
             (to-check data))
    (cond ((not inter) #f)
          ((null? to-check) inter)
          (else (loop (update-interval inter
                                       (fn (car to-check))
                                       (- (fn (cdddar to-check)) v))
                      (cdr to-check))))))

(define (next-valid-v fn start-v)
  (let ((inter (make-inter fn start-v)))
    (if inter (list start-v inter) (next-valid-v fn (next-v start-v)))))

(define (next-valid-vvv prev-vx prev-vy prev-vz)
  (let ((vx (next-v prev-vx))
        (vy (next-v prev-vy))
        (vz (next-v prev-vz)))
    (let ((inter-x (make-inter car vx)))
      (if inter-x
          (list 'x vx inter-x)
          (let ((inter-y (make-inter cadr vy)))
            (if inter-y
                (list 'y vy inter-y)
                (let ((inter-z (make-inter caddr vz)))
                  (if inter-z
                      (list 'z vz inter-z)
                      (next-valid-vvv vx vy vz)))))))))

(define (hail-valid? dpx dvx dpy dvy dpz dvz)
  (cond ((= 0 dvx dvy dvz) (= 0 dpx dpy dpz))
        ((= 0 dvy dvz)
          (assert (= 0 (remainder dpx dvx)))
          (assert (>= 0 (quotient dpx dvx)))
          (= 0 dpy dpz))
        ((= 0 dvx dvz)
          (assert (= 0 (remainder dpy dvy)))
          (assert (>= 0 (quotient dpy dvy)))
          (= 0 dpx dpz))
        ((= 0 dvx dvy)
          (assert (= 0 (remainder dpz dvz)))
          (assert (>= 0 (quotient dpz dvz)))
          (= 0 dpx dpy))
        ((= 0 dvx)
          (assert (= 0 (remainder dpy dvy)))
          (assert (= 0 (remainder dpz dvz)))
          (assert (>= 0 (quotient dpy dvy)))
          (assert (>= 0 (quotient dpz dvz)))
          (and (= 0 dpx)
               (= (quotient dpy dvy)
                  (quotient dpz dvz))))
        ((= 0 dvy)
          (assert (= 0 (remainder dpx dvx)))
          (assert (= 0 (remainder dpz dvz)))
          (assert (>= 0 (quotient dpx dvx)))
          (assert (>= 0 (quotient dpz dvz)))
          (and (= 0 dpy)
               (= (quotient dpx dvx)
                  (quotient dpz dvz))))
        ((= 0 dvz)
          (assert (= 0 (remainder dpx dvx)))
          (assert (= 0 (remainder dpy dvy)))
          (assert (>= 0 (quotient dpx dvx)))
          (assert (>= 0 (quotient dpy dvy)))
          (and (= 0 dpz)
               (= (quotient dpx dvx)
                  (quotient dpy dvy))))
        (else
          (assert (= 0 (remainder dpx dvx)))
          (assert (= 0 (remainder dpy dvy)))
          (assert (= 0 (remainder dpz dvz)))
          (assert (>= 0 (quotient dpx dvx)))
          (assert (>= 0 (quotient dpy dvy)))
          (assert (>= 0 (quotient dpz dvz)))
          (= (quotient dpx dvx)
             (quotient dpy dvy)
             (quotient dpz dvz)))))

(define (start-pos-2 lx ly lz)
  (let ((vx (car lx))
        (vy (car ly))
        (vz (car lz))
        (inter-px (cadr lx))
        (inter-py (cadr ly))
        (inter-pz (cadr lz)))
    (let loop ((px (car inter-px))
               (py (car inter-py))
               (pz (car inter-pz))
               (to-check data))
      (cond ((> pz (caddr inter-pz))
                (loop px (+ py (max 1 (cadr inter-py))) (car inter-pz) data))
            ((> py (caddr inter-py))
                (loop (+ px (max 1 (cadr inter-px)))
                      (car inter-py)
                      (car inter-pz)
                      data))
            ((> px (caddr inter-px))
                #f)
            ((null? to-check) (list px py pz))
            (else
              (let ((hail-px (car   (car to-check)))
                    (hail-py (cadr  (car to-check)))
                    (hail-pz (caddr (car to-check)))
                    (hail-vx (car   (cdddar to-check)))
                    (hail-vy (cadr  (cdddar to-check)))
                    (hail-vz (caddr (cdddar to-check))))
                (if (hail-valid? (- hail-px px) (- hail-vx vx)
                                 (- hail-py py) (- hail-vy vy)
                                 (- hail-pz pz) (- hail-vz vz))
                    (loop px py pz (cdr to-check))
                    (loop px py (+ pz (max 1 (cadr inter-pz))) data))))))))

(define (list-product l1 l2 l3)
  (let loop ((r1 l1) (r2 l2) (r3 l3) (acc '()))
    (cond ((null? r1) acc)
          ((null? r2) (loop (cdr r1) l2 l3 acc))
          ((null? r3) (loop r1 (cdr r2) l3 acc))
          (else (loop r1 r2 (cdr r3)
                      (cons (list (car r1) (car r2) (car r3)) acc))))))

(define answer-2
  (let ((start-vx (next-valid-v car   0))
        (start-vz (next-valid-v caddr 0))
        (start-vy (next-valid-v cadr  0)))
    (let loop ((all-vx (list start-vx))
               (all-vy (list start-vy))
               (all-vz (list start-vz))
               (todo   (list (list start-vx start-vy start-vz))))
      (let ((result (start-pos-2 (caar todo) (cadar todo) (caddar todo))))
        (cond (result result)
              ((not (null? (cdr todo)))
                 (loop all-vx all-vy all-vz (cdr todo)))
              (else (let ((new-v (next-valid-vvv (caar all-vx)
                                                 (caar all-vy)
                                                 (caar all-vz))))
                (cond ((eqv? (car new-v) 'x)
                          (loop (cons (cdr new-v) all-vx)
                                all-vy
                                all-vz
                                (list-product (list (cdr new-v))
                                              all-vy
                                              all-vz)))
                      ((eqv? (car new-v) 'y)
                          (loop all-vx
                                (cons (cdr new-v) all-vy)
                                all-vz
                                (list-product all-vx
                                              (list (cdr new-v))
                                              all-vz)))
                      ((eqv? (car new-v) 'z)
                          (loop all-vx
                                all-vy
                                (cons (cdr new-v) all-vz)
                                (list-product all-vx
                                              all-vy
                                              (list (cdr new-v)))))
                      (else (assert #f))))))))))

(write-line (conc "Second puzzle: " (apply + answer-2)))
