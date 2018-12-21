#lang racket
(define firsts
  (lambda (l)
    (cond
      ((null? l) '()),
      (else
       (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda ( new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new ( insertR new old (cdr lat)))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))



(define insertL
  (lambda (new old lat)
    (cond
      ((null lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else
        (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new old (cdr lat)))))))
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define mutirember
  (lambda ( n lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) n) (mutirember n (cdr lat)))
      (else
       (cons (car lat) (mutirember n (cdr lat)))))))

(define mutiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (mutiinsertR new old (cdr lat)))))
      (else
       (cons (car lat) (mutiinsertR new old (cdr lat)))))))
(define mutiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (mutiinsertL new old (cdr lat)))))
      (else
       (cons (car lat) (mutiinsertL new old (cdr lat)))))))
(define mutisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (mutisubst new old (cdr lat))))
      (else
       (cons (car lat) (mutiinsertL new old (cdr lat)))))))
