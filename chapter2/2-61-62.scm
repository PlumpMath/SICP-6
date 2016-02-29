(define (element-of-set? x set)
  (cond ((null? set) false)
        ((< x (car set)) false)
        ((= x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        (else
            (cond ((= (car s1) (car s2)) (cons (car s1) (intersection-set (cdr s1) (cdr s2))))
                  ((< (car s1) (car s2)) (intersection-set (cdr s1) s2))
                  ((> (car s1) (car s2)) (intersection-set s1 (cdr s2)))))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
          (cond ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
                ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
                ((> (car s1) (car s2)) (cons (car s2) (union-set s1 (cdr s2))))))))
