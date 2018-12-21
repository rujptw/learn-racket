#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(atom? '(2 3))
