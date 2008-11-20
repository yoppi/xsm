(use srfi-1)

(define (main args)
  (print args)
  (print (number? (string->number (second args)))))
