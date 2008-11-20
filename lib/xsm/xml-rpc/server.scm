(define-module xsm.xml-rpc.server
  (extend xsm.xml-rpc.common)
  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use rfc.uri)
  (use util.list)
  (use text.tree)
  (use gauche.net)
  (use xsm.xml-rpc.marshal)
  (use xsm.xml-rpc.http)
  (export make-mount-table))
(select-module xsm.xml-rpc.server)

(define-class <xml-rpc-server-mount-table> ()
  ((table :accessor table-of :init-form (make-hash-table 'string=?))))

(define (mount mount-table name value)
  (hash-table-put! (table-of mount-table) name value))

(define (make-mount-table bindings)
  (let ((mount-table (make <xml-rpc-server-mount-table>)))
    (let loop ((bindings bindings))
      (unless (null? bindings)
        (apply mount mount-table (car bindings))
        (loop (cdr bindings))))
    mount-table))

(define (handle-request mount-table name . args)
  (with-error-handler
      (lambda (e)
        (make-fault-response 444 ;;; Uhmm...
                             (ref e 'message)))
    (lambda ()
      (make-success-response
       (apply (hash-table-get (table-of mount-table) name)
              args)))))

(define (make-response content)
  (call-with-output-string
    (lambda (output)
      (sxml->xml
       '()
       `(*TOP*
         (*PI* xml
               ("version" "1.0")
               ("encoding" ,(symbol->string (gauche-character-encoding))))
         (methodResponse ,content))
       output))))

(define (make-success-response result)
  (make-response 
   `(params
     (param ,(marshal-value result)))))

(define (make-fault-response code phrase)
  (make-response 
   `(fault
     ,(marshal-value (alist->hash-table `((faultCode . ,code)
                                          (faultString . ,phrase)))))))

(provide "xsm/xml-rpc/server")
