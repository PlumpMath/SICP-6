; Global table
(define tbl (list))

; Find element in (k,v) array
(define (get-by-key key array)
  (cond ((null? array) '())
        ((eq? (caar array) key) (cadar array))
        (else (get-by-key key (cdr array)))))

; Replace element in (k,v) array
(define (replace key value array)
  (define (merge array1 array2)
    (cond ((null? array1) array2)
          (else (merge (cdr array1) (cons (car array1) array2)))))
  (define (replace-helper key value array new-array)
    (cond ((null? array) (cons new-array (list key value)))
          ((eq? (caar array) key) (merge new-array (cons (list key value) (cdr array))))
          (else (replace-helper key value (cdr array) (merge new-array (list (car array)))))))
  (replace-helper key value array (list)))

; Access 2D global table
(define (get row col)
  (get-by-key col (get-by-key row tbl)))

; Put elment in 2D global table
(define (put row col value)
  (cond ((null? (get-by-key row tbl))
         ; add a new row
         (set! tbl (cons (list row (list (list col value))) tbl)))
        (else
          (let ((the-row (get-by-key row tbl)))
          (cond ((null? (get-by-key col the-row))
                 ; replace the row by appending the new pair
                 (set! tbl (replace row (cons (list col value) the-row) tbl)))
                (else
                 ; replace the row by replacing the element in the row
                 (set! tbl (replace row (replace col value the-row) tbl))))))))


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (install-deriv-package)
 ;;internal procedures
 (define (=number? exp num) (and (number? exp) (= exp num)))
 (define (make-sum a1 a2)
   (cond ((=number? a1 0) a2)
         ((=number? a2 0) a1)
         ((and (number? a1) (number? a2)) (+ a1 a2))
         (else (list '+ a1 a2))))
 (define (make-product m1 m2)
   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
         ((=number? m1 1) m2)
         ((=number? m2 1) m1)
         ((and (number? m1) (number? m2)) (* m1 m2))
         (else (list '* m1 m2))))
 (define (make-exponentiation b e)
   (cond ((=number? e 0) 1)
         ((=number? e 1) b)
         (else (list '** b e))))
 (define (sum? x) (and (pair? x) (eq? (car x) '+)))
 (define (addend s) (cadr s))
 (define (augend s)
   (cond ((null? (cdr (cddr s))) (caddr s))
         (else (cons '+ (cddr s)))))
 (define (product? x) (and (pair? x) (eq? (car x) '*)))
 (define (multiplier p) (cadr p))
 (define (multiplicand p)
   (cond ((null? (cdr (cddr p))) (caddr p))
         (else (cons '* (cddr p)))))
 (define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
 (define (base p) (cadr p))
 (define (exponent p) (caddr p))

 (define (deriv-sum exp var)
   (make-sum (deriv (addend exp) var)
             (deriv (augend exp) var)))
 (define (deriv-product exp var)
   (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
 (define (deriv-exponentiation exp var)
   (make-product
           (make-product
             (exponent exp)
             (make-exponentiation (base exp) (- (exponent exp) 1)))
           (deriv (base exp) var)))

 ;;interface to the rest of the system
 (put 'deriv '+ deriv-sum)
 (put 'deriv '* deriv-product)
 (put 'deriv '** deriv-exponentiation)

 ;;a second set of interface
 (put '+ 'deriv deriv-sum)
 (put '* 'deriv deriv-product)
 (put '** 'deriv deriv-exponentiation)
 'done)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
 (cond ((number? exp) 0)
       ((variable? exp) (if (same-variable? exp var) 1 0))
       (else ((get (operator exp) 'deriv)
              exp var))))
