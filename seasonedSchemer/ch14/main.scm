(define atom?
  (lambda (s)
    (and (not (pair? s)) (not (null? s)))))

#|
(define leftmost
  (lambda (l)
      (cond ((null? l) '())
            ((atom? (car l)) (car l))
            (else
             (let ((a (leftmost (car l))))
               (cond ((atom? a) a)
                     (else (leftmost (cdr l)))))))))
|#
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

#| cf hard to know nested depth ver
(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          (else 
           (let ((d (depth* (cdr l))))
             (cond ((atom? (car l)) d)
                   (else
                    (let ((a (depth* (car l))))
                      (cond ((> d (+ a 1)) d)
                            (else
                             (+ a 1)))))))))))
|#

#|
(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l))
           (depth* (cdr l)))
          (else
           (let ((a (depth* (car l)))
                 (d (depth* (cdr l))))
             (cond ((> d (+ a 1)) d)
                   (else (+ a 1))))))))
|#

(define maxx
  (lambda (a b)
    (if (> a b) a b)))

#| more beautiful
(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l))
           (depth* (cdr l)))
          (else
           (let ((a (+ (depth* (car l)) 1))
                 (d (depth* (cdr l))))
             (max a d))))))
|#

; most beautiful! perfect!
(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l))
           (depth* (cdr l)))
          (else
           (max
            (+ (depth* (car l)) 1)
            (depth* (cdr l)))))))

(define pick
  (lambda (a lat)
    (cond ((= a 1) (car lat))
          (else
           (pick (- a 1) (cdr lat))))))

; revised scramble
(define scramble
  (lambda (tup)
    (letrec
        ((S (lambda (tup pre)
              (cond ((null? tup) '())
                    (else
                     (let ((next-pre (cons (car tup) pre)))
                     (cons (pick (car tup) next-pre)
                           (S (cdr tup) next-pre))))))))
      (S tup '()))))

; revised leftmost
(define leftmost
  (lambda (l)
    (call-with-current-continuation
     (lambda (return)
       (letrec
           ((lm (lambda (l)
                  (cond
                    ((null? l) '())
                    ((atom? (car l))
                     (return (car l)))
                    (else
                     (let ()
                       (lm (car l))
                       (lm (cdr l))))))))
         (lm l))))))

; so so difficult!
(define rember1**
  (lambda (a l oh)
    (cond
      ((null? l) (oh 'no))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rember1** a (cdr l) oh))))
      (else
       (if (atom?
            (call-with-current-continuation
             (lambda (oh)
               (rember1** a (car l) oh))))
           (cons (car l)
                 (rember1** a (cdr l) oh))
           (cons (rember1** a (car l) oh)
                 (cdr l)))))))



(leftmost '((swedish rye) (french (mustard salad turkey)) salad))
(rember1** 'salad '((swedish rye) (french (mustard salad turkey)) salad) 1)
(depth* '(c (b (a b) a) a))
(scramble '(1 1 1 3 4 2 1 1 9 2))
(rember1** 'Say '((food) more (food)) 1)