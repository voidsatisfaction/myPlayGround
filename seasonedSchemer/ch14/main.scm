(define atom?
  (lambda (s)
    (and (not (pair? s)) (not (null? s)))))

(define leftmost
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) (car l))
          (else
           (cond ((atom? (leftmost (car l)))
                  (leftmost (car l)))
                 (else
                  (leftmost (cdr l))))))))

(leftmost '((((()) ((a)) 2) a) c))