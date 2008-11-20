#!/usr/bin/env gosh

(define-module xml-rpc-server-test
  (use test.unit)
  (use util.list)
  (extend xsm.xml-rpc.server))
(select-module xml-rpc-server-test)

(define-test-case "XML-RPC server test"
  ("make-mount-point test"
   (assert-lset-equal '(("a" . b) ("b" . c) ("c" . d))
                      (hash-table->alist
                       (table-of
                        (make-mount-table
                         '(("a" b) ("b" c) ("c" d))))))))
