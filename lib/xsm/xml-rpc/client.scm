(define-module xsm.xml-rpc.client
  (extend xsm.xml-rpc.common)
  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use rfc.uri)
  (use text.tree)
  (use gauche.net)
  (use xsm.xml-rpc.marshal)
  (use xsm.xml-rpc.http)
  (export make-xml-rpc-client
          xml-rpc-client-call call))
(select-module xsm.xml-rpc.client)

(define-class <xml-rpc-client> ()
  ((host :accessor host-of :init-keyword :host)
   (port :accessor port-of :init-keyword :port)
   (path :accessor path-of :init-keyword :path)
   (timeout :accessor timeout-of :init-keyword :timeout)))

(define (make-xml-rpc-client uri . keywords)
  (receive (scheme user-info host port path query fragment)
      (uri-parse uri)
    (unless (equal? "http" scheme)
      (errorf "not supported scheme: <~a>" scheme))
    (unless host
      (errorf "host does not specified"))
    (unless path
      (errorf "path does not specified"))
    (make <xml-rpc-client>
      :host host
      :port (or port 80)
      :path path
      :timeout (get-keyword :timeout keywords '(0 500000)))))

(define (make-request name . args)
  (call-with-output-string
    (lambda (output)
      (sxml->xml
       '()
       `(*TOP*
         (*PI* xml
               ("version" "1.0")
               ("encoding" ,(symbol->string (gauche-character-encoding))))
         (methodCall
          (methodName ,(x->string name))
          (params ,@(map (lambda (arg)
                           `(param ,(marshal-value arg)))
                         args))))
       output))))

(define (xml-rpc-client-call client name . args)
  (let* ((socket (make-client-socket 'inet (host-of client) (port-of client)))
         (in (socket-input-port socket))
         (out (socket-output-port socket))
         (body (apply make-request name args))
         (headers `(("Host" ,(host-of client))
                    ("User-Agent" ,#`"xsm.xml-rpc.client/,|*xml-rpc-version*|")
                    ("Content-Type" "text/xml")
                    ("Content-Length" ,(number->string (string-size body))))))
    (dynamic-wind
        (lambda () #f)
        (lambda ()
          (http-request (path-of client) headers body out)
          (http-response-parse in))
        (lambda ()
          (unless (eq? 'shutdown (socket-status socket))
            (socket-shutdown socket 2))))))

(define-method call ((self <xml-rpc-client>) name . args)
  (apply xml-rpc-client-call self name args))

(provide "xsm/xml-rpc/client")
