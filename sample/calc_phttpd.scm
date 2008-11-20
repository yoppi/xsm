#!/usr/bin/env gosh

(use phttpd)
(use xsm.xml-rpc.server.cgi)

(define phttpd
  (make <phttpd>
        :metavariables-base '(
                              ;("SERVER_ADDR" "211.10.15.202")
                              ;("SERVER_ADMIN" "yamada@hiyoko.tir.jp")
                              ;("SERVER_NAME" "d.tir.jp")
                              ;("SERVER_PORT" 80)
                              ;; etc...
                              )
        ;:listen-addr "0.0.0.0"
        :listen-addr "127.0.0.1"
        :listen-port 8080
        :thread #f
        ;:log-level 'warn ; debug,info,notice,warn,error,crit,alert,emerg,none
        ;:timeout 10 ; not yet
        ))

(define servlet-of-XML-RPC
  (let* ((count 0)
         (bindings `(("calc.add" ,(lambda (x y) (+ x y)))
                     ("calc.sub" ,(lambda (x y) (- x y)))
                     ("calc.multi" ,(lambda (x y) (* x y)))
                     ("calc.div" ,(lambda (x y) (/ x y)))
                     ("countup" ,(lambda ()
                                   (inc! count)
                                   count))))
         (mount-table (make-mount-table bindings)))
    (lambda (args)
      (xml-rpc-server-cgi-main mount-table))))

(define (main args)
  (add-servlet phttpd "/RPC2" servlet-of-XML-RPC)
  (kickstart phttpd))
