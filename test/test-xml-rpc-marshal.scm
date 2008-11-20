#!/usr/bin/env gosh

(define-module xml-rpc-marshal-test
  (use srfi-19)
  (use rfc.base64)
  (use test.unit)
  (use util.list)
  (use util.match)
  (extend xsm.xml-rpc.marshal))
(select-module xml-rpc-marshal-test)

(define-test-case "XML-RPC marshal test"
  ("marshal-value test"
   (assert-equal '(value (int "1")) (marshal-value 1))
   (assert-equal '(value (int "-5")) (marshal-value -5))
   (assert-equal `(value (int ,(x->string (- (* 256 256 256 128) 1))))
                 (marshal-value (- (* 256 256 256 128) 1)))
   (assert-error (lambda () (marshal-value (* 256 256 256 128))))
   (assert-equal `(value (int ,(x->string (- (* 256 256 256 128)))))
                 (marshal-value (- (* 256 256 256 128))))
   (assert-error (lambda () (marshal-value (- (+ 1 (* 256 256 256 128))))))
   
   (assert-equal '(value (boolean "0")) (marshal-value #f))
   (assert-equal '(value (boolean "1")) (marshal-value #t))

   (assert-equal '(value (string "abc")) (marshal-value "abc"))

   (assert-equal '(value (double "1.0")) (marshal-value 1.0))
   (assert-equal '(value (double "-3.14")) (marshal-value -3.14))
   
   (assert-equal '(value (dateTime.iso8601 "19980717T14:08:55"))
                 (marshal-value (make-date 0 55 8 14 17 7 1998 0)))

   (assert-equal `(value (base64
                          ,(base64-encode-string "abcde")))
                 (marshal-value (make-base64-encoded-string "abcde")))

   (match (marshal-value (alist->hash-table
                          '((a . 1) (b . "abc") (c . 1.0))
                          'eq?))
     ((%value (%struct . %members))
      (assert-equal 'value %value)
      (assert-equal 'struct %struct)
      (assert-lset-equal '((member (name "a")
                                   (value (int "1")))
                           (member (name "b")
                                   (value (string "abc")))
                           (member (name "c")
                                   (value (double "1.0"))))
                         %members))
     (value (assert-equal '(value (struct (member (name "a")
                                                  (value (int "1")))
                                          (member (name "b")
                                                  (value (string "abc")))
                                          (member (name "c")
                                                  (value (double "1.0")))))
                          value)))
   
    (assert-equal '(value (array (data (value (int "1"))
                                                    (value (string "abc"))
                                                    (value (double "-1.0")))))
                  (marshal-value '#(1 "abc" -1.0)))
    
    (assert-equal '(value
                     (array
                      (data (value (int "1"))
                            (value (array
                                    (data (value (string "abc"))
                                          (value (double "-1.0"))))))))
                  (marshal-value '#(1 #("abc" -1.0))))))
