#!/usr/bin/env gosh

(define-module xml-rpc-http-test
  (use test.unit)
  (use sxml.tools)
  (use text.tree)
  (use www.cgi)
  (use gauche.parameter)
  (extend xsm.xml-rpc.http))
(select-module xml-rpc-http-test)

(define (%http-response-header-read str)
  (http-response-header-read (open-input-string str)))

(define (%http-response-header-parse str)
  (http-response-header-parse (open-input-string str)))

(define-assertion (assert-http-error code phrase thunk)
  (call/cc
   (lambda (return)
     (let ((result
            (call/cc
             (lambda (break)
               (with-exception-handler
                   (lambda (e)
                     (if (is-a? e (with-module xsm.xml-rpc.http <http-error>))
                       (break e)
                       (return
                        (make-assertion-failure
                         (format
                          #f
                          " expected:<~s> class exception\n  but was:<~a> <~a>"
                          (with-module xsm.xml-rpc.http <http-error>)
                          (class-of e)
                          (ref e 'message))
                         e))))
                 (lambda ()
                   (thunk)
                   (return
                    (make-assertion-failure
                     (format
                      #f
                      " expected:<~s> class exception\n  but none was thrown"
                      (with-module xsm.xml-rpc.http <http-error>))))))))))
       (assert-equal code (ref result 'code))
       (assert-equal phrase (ref result 'phrase))))))

(define-test-case "xml-rpc.http test"
  ("http-response-header-read test"
   (assert-equal '()
                 (%http-response-header-read "HTTP/1.1 200 OK"))
   (assert-raise <not-supported-http-version>
                 (lambda () (%http-response-header-read "HTTP/0.1 200 OK")))
   (assert-http-error 500 "Internal Server Error"
                      (lambda ()
                        (%http-response-header-read
                         "HTTP/1.1 500 Internal Server Error")))

   (assert-lset-equal
    '(("CONNECTION" . "close")
      ("CONTENT_LENGTH" . "158")
      ("CONTENT_TYPE" . "text/xml")
      ("DATE" . "Fri, 17 Jul 1998 19:55:08 GMT")
      ("SERVER" . "UserLand Frontier/5.1.2-WinNT"))
    (%http-response-header-read
     (string-join (list "HTTP/1.1 200 OK"
                        "Connection: close"
                        "Content-Length: 158"
                        "Content-Type: text/xml"
                        "Date: Fri, 17 Jul 1998 19:55:08 GMT"
                        "Server: UserLand Frontier/5.1.2-WinNT")
                  "\n"))))
  
  ("http-response-header-parse test"
   (assert-equal 159
                 (%http-response-header-parse
                  (string-join (list "HTTP/1.1 200 OK"
                                     "Connection: close"
                                     "Content-Length: 159"
                                     "Content-Type: text/xml"
                                     "Date: Fri, 17 Jul 1998 19:55:08 GMT"
                                     "Server: UserLand Frontier/5.1.2-WinNT")
                               "\n")))
   (assert-raise
    <invalid-content-type>
    (lambda ()
      (%http-response-header-parse
       (string-join (list "HTTP/1.1 200 OK"
                        "Connection: close"
                        "Content-Length: 158"
                        "Content-Type: text/xml; charset=UTF-8"
                        "Date: Fri, 17 Jul 1998 19:55:08 GMT"
                        "Server: UserLand Frontier/5.1.2-WinNT")
                    "\n"))))
   (assert-raise
    <invalid-content-length>
    (lambda ()
      (%http-response-header-parse
       (string-join (list "HTTP/1.1 200 OK"
                        "Connection: close"
                        "Content-Length: aaa"
                        "Content-Type: text/xml"
                        "Date: Fri, 17 Jul 1998 19:55:08 GMT"
                        "Server: UserLand Frontier/5.1.2-WinNT")
                    "\n")))))
  ("xml-encoding test"
   (assert-equal "UTF-8"
                 (xml-encoding "<?xml version='1.0' encoding='UTF-8'?>"))
   (assert-false (xml-encoding "<?xml version='1.0'?>"))
   (assert-equal "euc-jp"
                 (xml-encoding "<?xml version='1.0' encoding=\"euc-jp\"?>")))
  ("http-response-body-read test"
   (assert-equal "<a/>"
                 (xml-read (open-input-string "<a/>")
                           4))
   (assert-equal "<b><a/></b>"
                 (xml-read (open-input-string "<b><a/></b>")
                           11)))
  ("http-response-body-parse test"
   (assert-values-equal
    '("South Dakota")
    (lambda ()
      (let ((xml (tree->string
                  (sxml:sxml->xml
                   '(methodResponse
                     (params
                      (param
                       (value (string "South Dakota")))))))))
        (http-response-body-parse (open-input-string xml)
                                  (string-size xml)))))
   (assert-error-message
    "XML-RPC FAULT: code=4; string=Too many parameters."
    (lambda ()
      (let ((xml (tree->string
                  (sxml:sxml->xml
                   '(methodResponse
                     (fault
                      (value
                       (struct
                        (member (name "faultCode")
                                (value (int "4")))
                        (member (name "faultString")
                                (value (string "Too many parameters.")))))))))))
        (http-response-body-parse (open-input-string xml)
                                  (string-size xml))))))
  ("http-response-body-parse test"
   (assert-values-equal
    '("South Dakota")
    (lambda ()
      (let* ((xml (tree->string
                   (sxml:sxml->xml
                   '(methodResponse
                     (params
                      (param
                       (value (string "South Dakota"))))))))
             (response #`"HTTP/1.1 200 OK
Content-Type: text/xml
Content-Length: ,(string-size xml)
Connection: close
Date: Fri, 17 Jul 1998 19:55:08 GMT
Server: UserLand Frontier/5.1.2-WinNT

,|xml|"))
        (http-response-parse (open-input-string response))))))
  ("http-request test"
   (let* ((xml "<?xml version=\"1.0\"?>
<methodCall>
  <methodName>examples.getStateName</methodName>
  <params>
    <param>
      <value><i4>41</i4></value>
      </param>
    </params>
  </methodCall>")
          (request #`"POST /RPC2 HTTP/1.1\r
User-Agent: Frontier/5.1.2 (WinNT)\r
Host: betty.userland.com\r
Content-Type: text/xml\r
Content-length: ,(string-size xml)\r
\r
,|xml|")
          (output (open-output-string)))
     (http-request "/RPC2"
                   `(("User-Agent" "Frontier/5.1.2 (WinNT)")
                     ("Host" "betty.userland.com")
                     ("Content-Type" "text/xml")
                     ("Content-length" ,(x->string (string-size xml))))
                   xml
                   output)
     (assert-equal request (get-output-string output))))
  ("http-request-body-parse"
   (let ((xml "
<methodCall>
  <methodName>examples.getStateName</methodName>
  <params>
    <param>
      <value><i4>41</i4></value>
      </param>
    </params>
  </methodCall>"))
     (assert-values-equal
      '("examples.getStateName" (41))
      (lambda ()
        (http-request-body-parse (open-input-string xml)
                                 (string-size xml)))))
   (let ((xml "
<methodCall>
  <methodName>add</methodName>
  <params>
    <param>
      <value><i4>1</i4></value>
      </param>
    <param>
      <value><i4>-2</i4></value>
      </param>
    <param>
      <value><double>3.0</double></value>
      </param>
    </params>
  </methodCall>"))
     (assert-values-equal
      '("add" (1 -2 3.0))
      (lambda ()
        (http-request-body-parse (open-input-string xml)
                                 (string-size xml))))))
  ("http-request-parse test"
   (assert-http-error 405 "Method Not Allowed"
                      (lambda ()
                        (http-request-parse (open-input-string ""))))
   (assert-http-error 400 "Bad Request"
                      (lambda ()
                        (parameterize ((cgi-metavariables
                                        '(("REQUEST_METHOD" "POST"))))
                          (http-request-parse (open-input-string "")))))
   (assert-http-error 411 "Length Required"
                      (lambda ()
                        (parameterize ((cgi-metavariables
                                        '(("REQUEST_METHOD" "POST")
                                          ("CONTENT_TYPE" "text/xml")
                                          ("CONTENT_LENGTH" 0))))
                          (http-request-parse (open-input-string "")))))
   (assert-values-equal
    '("add" (1 -2 3.0))
    (lambda ()
      (let ((xml "
<methodCall>
  <methodName>add</methodName>
  <params>
    <param>
      <value><i4>1</i4></value>
      </param>
    <param>
      <value><i4>-2</i4></value>
      </param>
    <param>
      <value><double>3.0</double></value>
      </param>
    </params>
  </methodCall>"))
        (parameterize ((cgi-metavariables
                        `(("REQUEST_METHOD" "POST")
                          ("CONTENT_TYPE" "text/xml")
                          ("CONTENT_LENGTH" ,(string-size xml)))))
          (http-request-parse (open-input-string xml)))))))
  ("http-response test"
   (let ((xml (tree->string
               (sxml:sxml->xml
                '(methodResponse
                  (fault
                      (value
                       (struct
                        (member (name "faultCode")
                                (value (int "4")))
                        (member (name "faultString")
                                (value (string "Too many parameters."))))))))))
         (output (open-output-string)))
     (http-response `(("Status" "200 OK")
                      ("Content-Type" "text/xml")
                      ("Content-Length" ,(number->string (string-size xml))))
                    xml
                    output)
     (assert-equal #`"Status: 200 OK\r
Content-Type: text/xml\r
Content-Length: ,(string-size xml)\r
\r
,xml"
                   (get-output-string output)))))
