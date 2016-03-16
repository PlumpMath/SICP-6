(define (make-accumulator acc)
  (define (dispatch m) (begin (set! acc (+ acc m))
                              acc))
  dispatch)
