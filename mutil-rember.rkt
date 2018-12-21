#lang racket
(define  atom?
  (lambda  (x)
    (and  (not  (pair?  x))  (not  (null?  x)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else
          (cons (car l) (rember* a (cdr l))))))
      (else
       (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons old (cons new (insertR* new old (cdr lat)))))
         (else
          (cons (car lat) (insertR* new old (cdr lat))))))
      (else
       (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
         (else
          (occur* a (cdr l)))))
       (else
        (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else
          (cons (car l) (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l)) (subst* new old (cdr l)))))))



(define insertL*
  (lambda ( new old l )
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else
          (cons (car l) (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else
          (member* a (cdr l)))))
      (else
       (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else
       (leftmost (car l))))))

(define eqan?
  (lambda (a b)
      (cond
        ((and (number? a) (number? b) ( = a b)))
        ((or (number?  a) (number? b)) #f)
        (else
         (eq? a b)))))

(define eqlist
  (lambda (m n)
    (cond
      ((and (null? m) (null? n)) #t)
      ((or (null? m) (null? n))#f)
      ((and (atom? (car m)) (atom? (car n)))
       (cond
         ((and(eq? (car m) (car n)) (eqlist (cdr m) (cdr n))))
         (else
          #f)))
      (else
       (and (eqlist (car m) (car n)) (eqlist (cdr m) (cdr n)))))))

(eqlist '(beef ((sausage)) (or (soda))) '(beef ((sausage)) (and (soda))))
         
(define equal?
  (lambda ( n m )
    (cond
      ((and (atom? n) (atom? m)) (eqan? n m))
      ((or (atom? n) (atom? n)) #f)
      (else
       (eqlist n m)))))
(define eqlist?
  (lambda (n m)
    (cond
      ((and (null? n) (null? m)) #t)
      ((or (null? n) (null? m)) #f)
      (else
       (and (equal? (car n) (car m))
            (eqlist? (cdr n) (cdr m)))))))
      

       

