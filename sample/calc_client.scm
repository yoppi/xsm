#!/usr/bin/env gosh

(use rfc.uri)
(use xsm.xml-rpc.client)

(define host "localhost")
(define port 8080)
(define path "/RPC2")
;; (define port 80)
;; (define path "/~kou/xml-rpc/calc2.cgi")

(define (main args)
  (let ((server (make-xml-rpc-client (uri-compose :scheme "http" :host host
						  :port port :path path)))
        (prev-handler (current-exception-handler)))
    (with-exception-handler
        (lambda (e)
          (use gauche.interactive)
          (d e)
          (prev-handler e))
      (lambda ()
        (print (xml-rpc-client-call server "calc.add" 1 2))
        (print (call server "calc.sub" 1 2))
        (print (xml-rpc-client-call server "calc.multi" 1.0 2.0))
        (print (call server "calc.div" 1.0 2.0))
        '(print (call server "countup"))))))
