(define (list? obj)
  (or (null? obj)
      (and (pair? obj) (cdr obj))))
