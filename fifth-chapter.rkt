#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define add1
    (lambda (n)
        (+ n 1)))

(define rember*
    (lambda (x lat)
    (cond
        ((null? lat) '())
        ((atom? (car lat))
            (cond
                ((eq? (car lat) x) (rember* x (cdr lat)))
                (else
                    (cons (car lat) (rember* x (cdr lat))))))
        (else
            (cons (rember* x (car lat)) (rember* x (cdr lat)))))))




(define eqan?
  (lambda (n m)
  (cond
    ((and (number? n) (number? m)) (= n m))
    ((or (number? n) (number? m)) #f)
    (else
      (eq? n m)))))


;(define x 'sauce)
;(define lat '(((tomato sauce))
;((bean) sauce)
;(and ((flying)) sauce)))
;(rember* x lat)

(define insertR
    (lambda (old new lat)
    (cond
        ((null? lat) '())
        ((atom? (car lat))
        (cond
            ((eq? (car lat) old) (cons old (cons new (insertR old new (cdr lat)))))
            (else
                (cons (car lat) (insertR old new (cdr lat))))))
        (else
            (cons (insertR old new (car lat)) (insertR old new (cdr lat)))))))
; (define new 'roast)
; (define old 'chuck)
; (define lat '((how  much  (wood)) could ((a  (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
; (insertR old new lat)

(define occur*
    (lambda (x lat)
    (cond
        ((null? lat) 0)
        ((atom? (car lat))
        (cond
            ((eq? (car lat) x) (add1 (occur* x (cdr lat))))
            (else
                (occur* x (cdr lat)))))
        (else
            (+ (occur* x (car lat)) (occur* x (cdr lat)))))))
; (define n 'banana)
; (define lats '((banana)
; (split ((((banana  ice)))
; (cream (banana))
; sherbet))
; (banana)
; (bread)
; (banana brandy)))
; (occur* n lats)

(define subst*
    (lambda (old new lat)
        (cond
            ((null? lat) '())
            ((atom? (car lat))
            (cond
                ((eq? (car lat) old) (cons new (subst* old new (cdr lat))))
                (else
                    (cons (car lat) (subst* old new (cdr lat))))))
            (else
                (cons (subst* old new (car lat)) (subst* old new (cdr lat)))))))
; (define new 'orange)
; (define old 'banana)
; (define lats '((banana)(split ((((banana ice))) (cream  (banana)) sherbet) (banana) (bread)(banana brandy))))
; (subst* old new lats)

(define insertL
    (lambda (old new lat)
    (cond
        ((null? lat) '())
        ((atom? (car lat))
        (cond
            ((eq? (car lat) old) (cons new (cons old (insertL old new (cdr lat)))))
            (else
                (cons (car lat) (insertL old new (cdr lat))))))
        (else
         (cons (insertL old new (car lat)) (insertL old new (cdr lat)))))))

;(define new 'roast)
;(define old 'chuck)
;(define lats '((how  much  (wood)) could ((a  (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
;(insertL old new lats)

(define member*
  (lambda (n lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat))
       (cond
         ((eq? (car lat) n) #t)
         (else
          (member* n (cdr lat)))))
      (else
       (or (member* n (car lat)) (member* n (cdr lat)))))))

;(define lat '((potato)  (chips ((with) fish) (chips))))
;(define n 'fish)
;(member* n lat)
(define leftmost
  (lambda (lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat)) (car lat))
      (else
       (leftmost (car lat))))))
(leftmost ' ((potato) (chips ((with) fish) (chips))))

(define equal?
    (lambda (n m)
    (cond
        ((and (atom? n) (atom? m)) (eqan? n m) )
        ((or (atom? n) (atom? m)) #f)
        (else
            (eqlist (n m))))))

; 原版eqlist
(define eqlist
    (lambda  (n m)
    (cond
        ((and (null? n) (null? m)) #t)
        ((or (null? n) (null? m)) #f)
        ((and(atom? (car l)) (atom? (car m)))
        (cond
            ((and (eqan? (car n) (car m)) (eqlist (cdr n) (cdr m))))))
        ((or (atom? (car n) (atom? m)) #f))
        (else
            (and (eqlist (car n) (car m)) (eqlist (cdr n) (cdr m)))))))




;帮助函数作用很大，让程序变得优雅(简化版eqlist)
(define eqlist
  (lambda (lat1 lat2)
    (cond
      ((and (null? lat1) (null? lat2)) #t)
      ((or (null? lat1) (null? lat2) #f))
      (else
        (and (equal? (car lat1) (car lat2)) (eqlist (cdr lat1) (cdr lat2)))))))


(define lat1  '(beef ((sausage)) (and (soda))))
(define lat2  '(beef ((sausage)) (and (soda))))
(eqlist lat1 lat2)

;简化版rember(接收atom和list,移除第一个和s相同的元素)
(define  rember
  (lambda  (s  I)
    (cond
    ((null?  I)  (quote  ()))
     ((equal? (car  I)  s) (cdr  1))
        (else  (cons (car  I) (rember  s  (cdr I)))))))
