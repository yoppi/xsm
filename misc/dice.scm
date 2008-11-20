;(define (try n times)
;  (cond [(= times 0) n]
;        [else (try (* n n) (- times 1))]))

(define (try n times)
  (define (iter total n times)
    (if (= times 0) 
      total
      (iter (* total n) n (- times 1))))
  (iter 1 n times))

(print (- 1 (try (/ 35.0 36.0) 24)))
