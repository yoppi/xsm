(define-module xsm.xml-rpc.parser
  (use srfi-2)
  (use srfi-19)
  (use util.record)
  (use rfc.base64)
  (use sxml.tools)
  (export parse-response parse-request))
(select-module xsm.xml-rpc.parser)

(define (first-content contents converter)
  (define (invalid)
    (errorf "invalid content <~a>" contents))

  (if contents
    (cond ((and (pair? contents)
                (null? (cdr contents)))
           (converter (car contents)))
          (else (invalid)))
    (invalid)))

(define-class <xr-element> ()
  ((value :accessor value-of)
   (contents :accessor contents-of :init-keyword :contents)))

(define-method initialize ((self <xr-element>) args)
  (next-method))

(define-class <xr-int> (<xr-element>)
  ())

(define <xr-i4> <xr-int>)

(define-method initialize ((self <xr-int>) args)
  (next-method)
  (set! (value-of self)
        (first-content (contents-of self)
                       (lambda (value)
                         (let ((num (string->number value)))
                           (if (integer? num)
                             num
                             (errorf "<int>'content (~a) must be integer"
                                     value)))))))

(define-class <xr-boolean> (<xr-element>)
  ())

(define-method initialize ((self <xr-boolean>) args)
  (next-method)
  (set! (value-of self)
        (first-content (contents-of self)
                       (lambda (value)
                         (cond ((string=? "0" value) #f)
                               ((string=? "1" value) #t)
                               (else (error "<boolean> must be 0 or 1")))))))

(define-class <xr-string> (<xr-element>)
  ())

(define-method initialize ((self <xr-string>) args)
  (next-method)
  (set! (value-of self)
        (first-content (contents-of self) identity)))

(define-class <xr-double> (<xr-element>)
  ())

(define-method initialize ((self <xr-double>) args)
  (next-method)
  (set! (value-of self)
        (first-content (contents-of self)
                       (lambda (value)
                         (let ((num (string->number value)))
                           (if (rational? num)
                             (+ num 0.0)
                             (errorf "<double>'content (~a) must be double"
                                     value)))))))

(define-class <xr-dateTime.iso8601> (<xr-element>)
  ())

(define-method initialize ((self <xr-dateTime.iso8601>) args)
  (next-method)
  (set! (value-of self)
        (first-content (contents-of self)
                       (lambda (value)
                         (with-error-handler
                             (lambda (e)
                               (with-output-to-port (current-error-port)
                                 (print (ref e 'message)))
                               (errorf "<dateTime.iso8601>'content (~a) must be dateTime formatted by iso8601"
                                       value))
                           (lambda ()
                             (let ((date (string->date value "~Y~m~dT~H:~M:~S")))
                               (make-date 0
                                          (date-second date)
                                          (date-minute date)
                                          (date-hour date)
                                          (date-day date)
                                          (date-month date)
                                          (date-year date)
                                          0))))))))

(define-class <xr-base64> (<xr-element>)
  ())

(define-method initialize ((self <xr-base64>) args)
  (next-method)
  (set! (value-of self)
        (first-content (contents-of self) base64-decode-string)))

(define-class <xr-struct> (<xr-element>)
  ())

;;; for struct->record version
(define-method initialize ((self <xr-struct>) args)
  (next-method)
  (let* ((members (parse-members (contents-of self)))
         (names (map car members))
         (values (map cadr members))
         (rtd (make-record-type #`"xr-struct-,(gensym)" names)))
    (set! (value-of self)
          (apply (record-constructor rtd names) values))))

;;; for struct->hash version
(define-method initialize ((self <xr-struct>) args)
  (next-method)
  (let* ((members (parse-members (contents-of self)))
         (names (map car members))
         (values (map cadr members))
         (hash-table (make-hash-table 'eq?)))
    (for-each (lambda (name value)
                (hash-table-put! hash-table name value))
              names
              values)
    (set! (value-of self) hash-table)))

(define-class <xr-array> (<xr-element>)
  ())

(define-method initialize ((self <xr-array>) args)
  (next-method)
  (let ((data (sxml:content (contents-of self))))
    (or (and-let* (((pair? data))
                   ((null? (cdr data)))
                   (data (car data))
                   ((eq? 'data (sxml:element-name data))))
          (set! (value-of self)
                (map parse-value (sxml:content data))))
        (errorf "subelement of <array> is must be one <data> element, but <~a>"
                data))))

(define (parse-member member)
  (let ((contents (sxml:content member)))
    (if (equal? '(name value)
                (map sxml:element-name contents))
      (receive (name value)
          (apply values contents)
        (list (first-content (sxml:content name) string->symbol)
              (parse-value value)))
      (errorf "invalid <member> ~s" member))))

(define (parse-members members)
  (map parse-member members))

(define (parse-value value)
  (unless (eq? 'value (sxml:element-name value))
    (errorf "must be single <value> element, but <~s>" value))
  (let* ((content (sxml:content value))
         (content (if (or (null? content)
                          (and (pair? (car content))
                               (null? (cdar content))))
                    "" ;; is it good?
                    (first-content content identity)))
         (name (sxml:element-name content))
         (klass (if name
                  (eval (string->symbol #`"<xr-,|name|>")
                        (current-module))
                  <xr-string>))
         (value-contents (if name
                           (sxml:content content)
                           (list content))))
    (value-of (make klass :contents value-contents))))

(define (parse-param param)
  (unless (eq? 'param (sxml:element-name param))
    (errorf "must be single <param> element, but <~s>" param))
  (parse-value (first-content (sxml:content param) identity)))

(define (parse-params params)
  (unless (eq? 'params (sxml:element-name params))
    (errorf "must be single <params> element, but <~s>" params))
  (map parse-param (sxml:content params)))

;;; client side
(define (parse-response sxml)
  (parse-method-response (first-content (sxml:content sxml) identity)))

(define (parse-method-response response)
  (unless (eq? 'methodResponse (sxml:element-name response))
    (errorf "must be single <methodResponse> element, but <~s>" response))
  (let* ((content (first-content (sxml:content response) identity))
         (name (sxml:element-name content)))
    (case name
      ((params) (apply values (parse-params content)))
      ((fault) (parse-fault content))
      (else (errorf "unknown subelement of <methodResponse>: <~a>"
                    name)))))

(define (parse-fault fault)
  (unless (eq? 'fault (sxml:element-name fault))
    (errorf "must be single <fault> element, but <~s>" fault))
  (let ((fault-info (parse-value (first-content (sxml:content fault)
                                                identity))))
    (errorf "XML-RPC FAULT: code=~a; string=~a"
            (hash-table-get fault-info 'faultCode)
            (hash-table-get fault-info 'faultString))))

;;; server side
(define (parse-request sxml)
  (parse-method-call (first-content (sxml:content sxml) identity)))

(define (parse-method-call request)
  (unless (eq? 'methodCall (sxml:element-name request))
    (errorf "must be single <methodCall> element, but <~s>" request))
  (let ((contents (sxml:content request)))
    (unless (and (pair? contents)
                 (pair? (cadr contents)))
      (errorf "too small contents <~a>" contents))
    (apply values
           (map (lambda (content handler)
                  (handler content))
                contents
                (list (lambda (content)
                        (parse-method-name content))
                      (lambda (content)
                        (parse-params content))
                      (lambda (content)
                        (errorf "too many content <~a>" content)))))))

(define (parse-method-name method-name)
  (unless (eq? 'methodName (sxml:element-name method-name))
    (errorf "must be single <methodName> element, but <~s>" method-name))
  (first-content (sxml:content method-name) identity))

(provide "xsm/xml-rpc/parser")
