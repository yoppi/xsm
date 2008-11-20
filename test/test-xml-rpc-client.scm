#!/usr/bin/env gosh

(define-module xml-rpc-client-test
  (use test.unit)
  (use sxml.ssax)
  (extend xsm.xml-rpc.client))
(select-module xml-rpc-client-test)

(define-test-case "XML-RPC client test"
  ("make-request test"
   (assert-equal `(*TOP*
                   (*PI* xml ,#`"version=\"1.0\" encoding=\",(gauche-character-encoding)\"")
                   (methodCall
                    (methodName "examples.getStateName")
                    (params
                     (param (value (int "41")))
                     (param (value (double "-41.41"))))))
                 (ssax:xml->sxml
                  (open-input-string
                   (make-request "examples.getStateName" 41 -41.41))
                  '()))))
