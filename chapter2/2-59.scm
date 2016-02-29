(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
        (else (cons x set))))

(define (intersection-set s1 s2)
  (cond ((null? s1) (list))
        ((element-of-set? (car s1) s2) (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
        (else (cons (car s1) (union-set (cdr s1) s2)))))
