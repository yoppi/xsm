(define-module xsm.xml-rpc.http
  (use srfi-13)
  (use text.tr)
  (use util.list)
  (use sxml.ssax)
  (use www.cgi)
  (use gauche.version)
  (use gauche.charconv)
  (use gauche.selector)
  (use xsm.xml-rpc.parser)
  (export http-request http-response-parse
          http-response http-request-parse
          http-error? code-of phrase-of))
(select-module xsm.xml-rpc.http)

(define-class <xml-rpc-http-error> (<exception>)
  ())

(define-class <not-supported-http-version> (<xml-rpc-http-error>)
  (version :accessor version-of :init-keyword :varsion))

(define-class <http-error> (<xml-rpc-http-error>)
  ((code :accessor code-of :init-keyword :code)
   (phrase :accessor phrase-of :init-keyword :phrase)))

(define (http-error? obj)
  (is-a? obj <http-error>))

(define-class <invalid-content-type> (<xml-rpc-http-error>)
  ((content-type :accessor content-type :init-keyword :content-type)))

(define-class <invalid-content-length> (<xml-rpc-http-error>)
  ((content-length :accessor content-length :init-keyword :content-length)))

(define (read-with-timeout input reader timeout not-response-handler)
  (if (char-ready? input)
    (reader input)
    (let ((result #f)
          (selector (make <selector>)))
      (selector-add! selector
                     input
                     (lambda (in . args)
                       (selector-delete! selector input #f #f)
                       (set! result (reader in)))
                     '(r))
      (let ((retry #f))
        (if (zero? (begin
                     (call/cc
                      (lambda (cont)
                        (set! retry cont)))
                     (selector-select selector timeout)))
          (not-response-handler retry)
          result)))))

(define (read-required-block input size eof-handler)
  (define (more-read size)
    (define retry-count 3)
    (read-with-timeout input (make-reader size) (list 3 0)
                       (lambda (retry)
                         (dec! retry-count)
                         (if (< retry-count 0)
                           (error "not response")
                           (retry)))))
  
  (define (make-reader size)
    (lambda (in . args)
      (read-block size in)))

  (define (read-more-if-need block)
    '(debug (list "read body" block))
    (cond ((eof-object? block) (eof-handler))
          ((< (string-size block) size)
           '(debug (list "more reading..." size (string-size block)
                        (- size (string-size block))))
           (read-more-if-need
            (string-append block
                           (more-read (- size (string-size block))))))
          (else
           '(debug (list "got block" block))
           block)))
    
  (read-more-if-need (more-read size)))

(define (xml-encoding str)
  (let ((md (#/^\s*<\?xml\s*.*\s*encoding=['\"]([^'\"]+)['\"].*\s*\?>/ str)))
    (and md (md 1))))
    
(define (xml-read input length)
  (let* ((block (read-required-block input length
                                     (lambda ()
                                       (error "content is too short"))))
         (encoding (xml-encoding (read-line (open-input-string block)))))
    (ces-convert block encoding)))


;;; client side
(define (http-request path headers body output)
  (format output "POST ~a HTTP/1.1\r\n" path)
  (for-each (lambda (header)
              (apply format output "~a: ~a\r\n" header))
            headers)
  (format output "\r\n")
  (format output "~a" body)
  (flush output))

(define (http-response-parse input)
  (http-response-body-parse input
                            (http-response-header-parse input)))

(define (http-response-header-parse input)
  (let* ((alist (http-response-header-read input))
         (content-type (assoc-ref alist "CONTENT_TYPE"))
         (content-length (assoc-ref alist "CONTENT_LENGTH")))
    ;(unless (equal? "text/xml" content-type)
    (unless (#/text\/xml/ content-type)
      (raise (make <invalid-content-type> :content-type content-type)))
    (unless (and content-length (string->number content-length))
      (raise (make <invalid-content-length> :content-length content-length)))

    (string->number content-length)))

(define (http-response-header-read input)
  (define counter 3)
  (define (next-line)
    (read-with-timeout input read-line (list 10 0)
                       (lambda (retry)
                         (dec! counter)
                         (if (> counter 0)
                           (retry)
                           (error "not response")))))

  (let ((status-line (next-line)))
    (rxmatch-let (rxmatch #/HTTP\/(\d+\.\d+) (\d+) (.+)$/ status-line)
        (#f version code phrase)
      (unless (and (version<=? "1.0" version)
                   (version<=? version "1.1"))
        (raise (make <not-supported-http-version> :version version)))
      (unless (= 200 (string->number code))
        ;; (print (list (string->number code) phrase))
        (raise (make <http-error>
                 :code (string->number code)
                 :phrase phrase)))))
  
  (let loop ((result '())
             (line (next-line)))
    (cond ((or (eof-object? line)
               (string-null? line))
           result)
          (else
           (loop (cons (let ((key&value (string-split line #/\s*:\s*/)))
                         (cons (string-tr (string-upcase (car key&value))
                                          "\-" "_")
                               (string-join (cdr key&value)
                                            ":")))
                       result)
                 (next-line))))))

(define (http-response-body-parse input length)
  (let ((body (xml-read input length)))
    ;; (print body)
    (parse-response (ssax:xml->sxml (open-input-string body) '()))))

;;; server side
(define (http-response headers body output)
  (for-each (lambda (header)
              (apply format output "~a: ~a\r\n" header))
            headers)
  (format output "\r\n")
  (format output "~a" body)
  (flush output))

(define (http-request-parse input)
  (unless (equal? "POST" (cgi-get-metavariable "REQUEST_METHOD"))
    (raise (make <http-error> :code 405 :phrase "Method Not Allowed")))
  (unless (#/^(text|application)\/xml$/
              (or (cgi-get-metavariable "CONTENT_TYPE") ""))
    (raise (make <http-error> :code 400 :phrase "Bad Request")))
  (let ((length (x->number (cgi-get-metavariable "CONTENT_LENGTH"))))
    (unless (< 0 length)
      (raise (make <http-error> :code 411 :phrase "Length Required")))
    (http-request-body-parse input length)))

(define (http-request-body-parse input length)
  (let ((body (xml-read input length)))
    (parse-request (ssax:xml->sxml (open-input-string body) '()))))

(provide "xsm/xml-rpc/http")
