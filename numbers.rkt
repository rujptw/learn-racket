#lang racket

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))


(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (add1 (add n (sub1 m)))))))

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (sub1 (sub n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       ( + (car tup) (addtup (cdr tup)))))))

(define cheng
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (+ n (cheng n (sub1 m)))))))

(define tup+
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      ((null? l2) l1)
      (else
       (cons (+ (car l1) (car l2)) (tup+ (cdr l1) (cdr l2)))))))
(define dayu
  (lambda (m n)
      (cond
        ((zero? m) #f)
        ((zero? n) #t)
        (else
         (dayu (sub1 m) (sub1 n))))))
(define xiaoyu
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (xiaoyu (sub1 m) (sub1 n))))))

(define dengyu
  (lambda (m n)
    (cond
      ((or (dayu m n) (xiaoyu m n)) #f)
      (else
       #t))))
(define mi
  (lambda (n m)
    (cond
      ((eq? 0 m) 1)
      (else
       (* n (mi n (sub1 m)))))))
(define division
  (lambda (n m)
      (cond
        ((< n m) 0)
        (else
         (add1 (division (- n m) m))))))
(division 6 5)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (length (cdr lat)))))))
(length '(3 4 5))

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) 0)
      (else
       ( + 1 (pick n (cdr lat)))
      
