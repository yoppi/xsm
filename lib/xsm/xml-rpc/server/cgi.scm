(define-module xsm.xml-rpc.server.cgi
  (extend xsm.xml-rpc.server)
  (use xsm.xml-rpc.http)
  (export xml-rpc-server-cgi-main))
(select-module xsm.xml-rpc.server.cgi)

(define-method xml-rpc-server-cgi-main ((bindings <list>))
  (%xml-rpc-server-cgi-main (make-mount-table bindings)))

(define-method xml-rpc-server-cgi-main
    ((mount-table <xml-rpc-server-mount-table>))
  (%xml-rpc-server-cgi-main mount-table))

(define (%xml-rpc-server-cgi-main mount-table)
  (call/cc
   (lambda (return)
     (let ((prev-handler (current-exception-handler)))
       (with-exception-handler
           (lambda (e)
             (cond ((http-error? e)
                    (http-response
                     `(("Status" ,#`",(code-of e) ,(phrase-of e)"))
                     ""
                     (current-output-port))
                    (return -1))
                   (else (prev-handler e))))
         (lambda ()
           (receive (name args)
               (http-request-parse (current-input-port))
             (let* ((body (apply handle-request mount-table name args))
                    (headers `(("Status" "200 OK")
                               ("Content-Type" "text/xml")
                               ("Content-Length"
                                ,(number->string (string-size body))))))
               (http-response headers body (current-output-port))
               0))))))))

(provide "xsm/xml-rpc/server/cgi")
