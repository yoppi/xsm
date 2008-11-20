(define-module xsm.xml-rpc.marshal
  (extend xsm.xml-rpc.common)
  (use srfi-19)
  (use gauche.sequence)
  (export marshal-value))
(select-module xsm.xml-rpc.marshal)

(define (make-value type content)
  `(value (,type ,@(if (pair? content)
                     content
                     (list content)))))

(define max-int (- (* 256 256 256 128) 1))
(define min-int (- (+ max-int 1)))

(define-method marshal-value ((value <integer>))
  (if (< value min-int)
    (errorf "too small int value: <~a>" value))
  (if (< max-int value)
    (errorf "too big int value: <~a>" value))
  (make-value 'int (x->string value)))

(define-method marshal-value ((value <boolean>))
  (make-value 'boolean (if value "1" "0")))

(define-method marshal-value ((value <string>))
  (make-value 'string value))

(define-method marshal-value ((value <real>))
  (make-value 'double (x->string value)))

(define-method marshal-value ((value <date>))
  (make-value 'dateTime.iso8601 (date->string value "~Y~m~dT~H:~M:~S")))

(define-method marshal-value ((value <base64>))
  (make-value 'base64 (encoded-string-of value)))

(define-method marshal-value ((value <hash-table>))
  (make-value 'struct
              (hash-table-fold value
                               (lambda (key val prev)
                                 (cons `(member (name ,(symbol->string key))
                                                ,(marshal-value val))
                                       prev))
                               '())))

(define-method marshal-value ((value <sequence>))
  (make-value 'array
              (list (cons 'data
                          (reverse! (fold (lambda (val prev)
                                            (cons (marshal-value val) prev))
                                          '()
                                          value))))))

(provide "xsm/xml-rpc/marshal")
