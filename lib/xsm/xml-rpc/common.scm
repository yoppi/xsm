(define-module xsm.xml-rpc.common
  (extend xsm.xml-rpc)
  (use srfi-11)
  (use srfi-13)
  (use rfc.base64)
  (use rfc.uri)
  (use sxml.tools)
  (use sxml.tree-trans)
  (export uri-parse sxml->xml
          make-base64-encoded-string encoded-string-of))
(select-module xsm.xml-rpc.common)

(define (uri-parse uri)
  (define (filter-non-empty-string str)
    (and (string? str)
         (not (string-null? str))
         str))

  (define (convert-if-not-false obj converter)
    (and obj (converter obj)))
  
  (receive (scheme specific)
      (uri-scheme&specific uri)
    (receive (authority path query fragment)
        (uri-decompose-hierarchical specific)
      (receive (user-info host port)
          (uri-decompose-authority authority)
        (values scheme
                user-info
                (filter-non-empty-string host)
                (convert-if-not-false port string->number)
                (filter-non-empty-string path)
                query
                fragment)))))

(define (sxml->xml local-rules sxml output)
  (define this-ss
    (append local-rules
            `((*TOP* . ,(lambda (trigger . value) value))
              (*PI* . ,(lambda (trigger pi-target . elems)
                         (list "<?" pi-target " "
                               (string-join
                                (map (lambda (elem)
                                       (string-join
                                        (list (x->string (car elem))
                                              #`"\",(sxml:string->xml (cadr elem))\"")
                                        "="))
                                     elems)
                                " ")
                               "?>"
                               #\newline)))
              (*default* *preorder* .
                         ,(lambda (tag . elems) 
                            (let*-values
                                (((attrs content)
                                  (if (and (pair? elems) (pair? (car elems))
                                           (eq? '@ (caar elems)))
                                    (values (car elems) (cdr elems))
                                    (values '() elems)))) ; no attributes
                              (entag tag
                                     (if (null? attrs)
                                       attrs
                                       (cdr (pre-post-order attrs this-ss)))
                                     (pre-post-order content this-ss)))))
              (*text* . ,(lambda (trigger str)
                           (if (string? str) (sxml:string->xml str) str))))))
  (with-output-to-port output
    (lambda ()
      (SRV:send-reply (pre-post-order sxml this-ss)))))

(define (entag tag attrs content)
  (if (null? content)
    (list #\< tag attrs "/>")
    (list #\< tag attrs #\> content "</" tag #\>)))


(define-class <base64> ()
  ((string :accessor string-of :init-keyword :string)
   (encoded-string :accessor encoded-string-of)))

(define-method initialize ((self <base64>) args)
  (next-method)
  (set! (encoded-string-of self)
        (base64-encode-string (string-of self))))

(define (make-base64-encoded-string str)
  (make <base64> :string str))

(provide "xsm/xml-rpc/common")
