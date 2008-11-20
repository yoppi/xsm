#!/usr/bin/env gosh

(use rfc.uri)
(use benchmark)
(use xsm.xml-rpc.client)

(define host "localhost")
(define port 8080)
(define path "/RPC2")
;; (define port 80)
;; (define path "/~kou/xml-rpc/calc2.cgi")

(define n 100)

(define (main args)
  (let ((server (make-xml-rpc-client (uri-compose :scheme "http" :host host
						  :port port :path path))))
    (bm (lambda (r)
          (report r (lambda ()
                      (dotimes (i n #f)
                        (xml-rpc-client-call server "calc.add" 1 2)))
                  :label "calc.add:"))
        :label-width 10)))
