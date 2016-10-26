(define atom?
  (lambda (s)
    (and (not (pair? s)) (not (null? s)))))

(define leftmost
  (lambda (l)
      (cond ((null? l) '())
            ((atom? (car l)) (car l))
            (else
             (let ((a (leftmost (car l))))
               (cond ((atom? a) a)
                     (else (leftmost (cdr l)))))))))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          (else
           (cond ((eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
                 (else #f))))))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond ((null? l) '())
                    ((atom? (car l))
                     (cond ((eq? a (car l)) (cdr l))
                           (else (cons (car l) (R (cdr l))))))
                    (else
                     (let ((av (R (car l))))
                       (cond
                         ((eqlist? (car l) av)
                          (cons (car l) (R (cdr l))))
                         (else
                          (cons av (cdr l))))))))))
      (R l))))

#| cf
(define rember1*
  (lambda (a l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eq? a (car l)) (cdr l))
                 (else (cons (car l) (rember1* a (cdr l))))))
          (else
           (cons (rember1* a (car l))
                 (rember1* a (cdr l)))))))
|#

(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth* (cdr l)))
          (else
           (cond ((> (depth* (cdr l)) (+ (depth* (car l)) 1))
                  (depth* (cdr l)))
                 (else
                  (+ (depth* (car l)) 1)))))))

(leftmost '((swedish rye) (french (mustard salad turkey)) salad))
(rember1* 'salad '((swedish rye) (french (mustard salad turkey)) salad))
(depth* '(c (b (a b) a) a))