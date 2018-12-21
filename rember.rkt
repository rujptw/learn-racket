#lang racket
(define member?
  (lambda ( a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))



(define rember?
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? a (car l)) (rember? a (cdr l)))
      (else
       (cons (car l) (rember? a (cdr l)))))))
(rember? 'sauce '(soy sauce and tomato sauce))
