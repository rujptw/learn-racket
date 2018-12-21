#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define add1
  (lambda (n)
    (+ n 1)))

; (add1 6)

(define sub1
  (lambda (n)
    (- n 1)))
(define jia
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else
       (add1 (jia (sub1 n) m))))))

; (jia 8 8)

(define jian
  (lambda ( m n)
    (cond
      ((zero? n) m)
      (else
       (sub1 (jian m (sub1 n)))))))

; (jian 5 4)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (+ (car tup) (addtup (cdr tup)))))))

; (addtup '(8 9 4))

(define cheng
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (+ n (cheng n (sub1 m)))))))

; (cheng 3 4)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define tup1 '(3 4 ))
(define tup2 '(3 4 5))
; (tup+ tup1 tup2)

(define biger
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (biger (sub1 n) (sub1 m))))))
; (biger 6 5)

(define smaller
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (smaller (sub1 n) (sub1 m))))))
; (smaller 2 3)

(define equal
  (lambda (n m)
    (cond
      ((smaller n m) #f)
      ((biger n m) #f)
      (else
       #t))))
; (equal 3 4)

(define mi
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
       (* n (mi n (sub1 m)))))))
; (mi 3 3)
(define chu
  (lambda (n m)
  (cond
    ((< n m) 0)
    (else
      (add1 (chu (- n m) m)))
  )))
  ; (chu 5 3)


(define length
  (lambda (lat)
  (cond
  ((null? lat) 0)
  (else
    (add1 (length (cdr lat)))))))

; (define lat '(2 3 4))
; (length lat)
(define pick
  (lambda (n lat)
  (cond
    ((zero? n) '())
    ((zero? (sub1 n)) (car lat))
    (else
      (pick (sub1 n) (cdr lat))))))
; (pick 3  '(lasagna spaghetti  ravioli
; macaroni meatball))



(define non-nums
  (lambda (lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (non-nums (cdr lat)))
    (else
      (cons (car lat) (non-nums (cdr lat)))))))
; (non-nums lat)

(define all-nums
  (lambda (lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
    (else
      (all-nums (cdr lat))))))
; (all-nums lat)

(define eqan?
  (lambda (n m)
  (cond
    ((and (number? n) (number? m)) (= n m))
    ((or (number? n) (number? m)) #f)
    (else
      (eq? n m)))))

; (eqan? 4 'u)

(define occur
  (lambda (n lat)
  (cond
    ((null? lat) 0)
    ((eqan? (car lat) n) (add1 (occur n (cdr lat))))
    (else
      (occur n (cdr lat))))))

;  (occur 'a '(a a a 3 4 4))

(define one?
  (lambda (n)
    (= n 1)))
(one? 1)

(define rempick
  (lambda (n lat)
  (cond
    ((one? n) (cdr lat))
    (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(define lat ' (5 pears 6 prunes 9 dates))
(rempick 3 lat)
